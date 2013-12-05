
module Expressions

export Regex

import Base: match, rsearch, length
using Nodes
export showerror, ParseError

abstract Expression # -ism

type ParseError <: Exception
    text::ASCIIString
    expr::Expression
    pos::Int64
end

# TODO: Only problem: When it throws the error, it doesn't actually
# show my custom error message. Gaaa
function showerror(io::IO, e::ParseError)
    print(io, escape_string("Rule $(e.expr.name) didn't match at '$(e.text[e.pos:min(end,e.pos+20)])' (line $(line(e)), column $(column(e))).'"))
end

line(e::ParseError) = 1 + count(e.text[1:e.pos]) do c c == '\n' end

function column(e::ParseError)
    lastnewline = rsearch(e.text, '\n', e.pos)
    lastnewline == 0 ? e.pos : e.pos - lastnewline + 1
end

# TODO: to test_expressions.jl
using Base.Test
type TestExpression <: Expression
    name
end
mye = ParseError("foo\nfoo\n", TestExpression("tst"), 4)
@test line(mye) == 2
@test column(mye) == 1
myf = ParseError("foo\nfoo\n", TestExpression("tst"), 3)
@test line(myf) == 1
@test column(myf) == 3
myio = IOString()
showerror(myio, myf)
seekstart(myio)
@test readline(myio) == "Rule tst didn't match at 'o\\nfoo\\n' (line 1, column 3).'"

function parse(expr::Expression, text::ASCIIString, pos::Int64=1)
    node = match(expr, text, pos)
    if node._end != length(text)
        error(escape_string("Incomplete parse, stopped at char $(node._end) of $(pos): $(text[node._end:min(end, node._end+100)])"))
    end
    node
end

function match(expr::Expression, text::ASCIIString, pos::Int64=1)
    err = ParseError(text, expr, pos)
    node = _match!(expr, text, pos, Dict(), err)
    if isempty(node)
        println("err.text: ", err.text)
        println("expr: ", expr)
        throw(err)
    end
    node
end

function _match!(expr::Expression, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    expr_id = object_id(expr)
    key = (expr_id, pos)
    if haskey(cache, key)
        node = cache[key]
    else
        node = _uncached_match(expr, text, pos, cache, err)
        cache[key] = node
    end

    if isempty(node) && pos >= err.pos && (
        Base.isempty(expr.name) || Base.isempty(err.expr.name))
        # TODO: ParseError?
        err.expr = expr
        err.pos = pos
    end

    node
end

function as_rule(expr::Expression)
    if !isempty(expr.name)
        return "$(expr.name) = $(_as_rhs(expr))"
    end
    return _as_rhs(expr)
end

function _as_rhs(expr::Expression)
    error("_as_rhs not implemented in "*string(typeof(expr))*" only subclasses")
end

type Literal <: Expression
    literal::ASCIIString
    name::ASCIIString

    function Literal(literal::ASCIIString; name::ASCIIString="")
        new(literal, name)
    end
end

function _uncached_match(literal::Literal, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    if beginswith(text[pos:], literal.literal)
        return Node(literal.name, text, pos, pos - 1 + length(literal.literal))
    end
    EmptyNode()  # "Empty" node
end

function _as_rhs(literal::Literal)
    literal.literal
end

type Regex <: Expression
    name::ASCIIString
    re::Base.Regex

    function Regex(pattern; name="", options="")
        @assert(length(pattern) > 0)
        @show pattern
        if pattern[1] != '^'
            pattern = "^" * pattern
        end

        new(name, Base.Regex(pattern, options))
    end
end

function _uncached_match(regex::Regex, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    m = match(regex.re, text[pos:])
    if isa(m, Nothing)
        return EmptyNode()
    end
    assert(m.offset == 1)  # should only match start of text
    Node(regex.name, text, pos, pos - 1 + length(m.match), match=m)
end

function _as_rhs(regex::Regex)
    "~\"$(re.pattern)\"$(re.options)"
end

abstract _Compound <: Expression
# has members

type Sequence <: _Compound
    members
    name

    function Sequence(members...; name="")
        new(collect(members), name)
    end
end

function _uncached_match(sequence::Sequence, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    new_pos = pos
    length_of_sequence = 0
    children = Node[]
    for m in sequence.members
        node = _match!(m, text, new_pos, cache, err)
        if isempty(node)
            return node
        end
        push!(children, node)
        new_pos += textlength(node)
        length_of_sequence += textlength(node)
    end
    # Hooray! We got through all the members!
    return Node(sequence.name, text, pos, pos + length_of_sequence - 1, children)
end

function _as_rhs(sequence::Sequence)
    return join(sequence.members, " ")
end

type OneOf <: _Compound
    members
    name

    function OneOf(members...; name="")
        new(collect(members), name)
    end
end

function _uncached_match(oneof::OneOf, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    for m in oneof.members
        node = _match!(m, text, pos, cache, err)
        if !isempty(node)
            return Node(oneof.name, text, pos, node._end, [node])
        end
    end
    return EmptyNode()
end

function _as_rhs(e::OneOf)
    join(e.members, " / ")
end

type Lookahead <: _Compound
    members
    name

    function Lookahead(members...; name="")
        new(collect(members), name)
    end
end

function _uncached_match(self::Lookahead, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    node = _match!(self.members[1], text, pos, cache, err)
    if !isempty(node)
        return Node(self.name, text, pos, pos - 1)
    end
    return EmptyNode()
end

function _as_rhs(e::Lookahead)
    "&" * join(e.members, " / ")
end

type Not <: _Compound
    members
    name

    function Not(members...; name="")
        new(collect(members), name)
    end
end

function _uncached_match(self::Not, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    node = _match!(self.members[1], text, pos, cache, err)
    if isempty(node)
        return Node(self.name, text, pos, pos - 1)
    end
    return EmptyNode()
end

function _as_rhs(e::Not)
    "!" * join(e.members, " ")
end

type Optional <: _Compound
    members
    name

    function Optional(members...; name="")
        new(collect(members), name)
    end
end

function _uncached_match(self::Optional, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    node = _match!(self.members[1], text, pos, cache, err)
    if isempty(node)
        return Node(self.name, text, pos, pos - 1)
    end
    return Node(self.name, text, pos, node._end, [node])
end

function _as_rhs(e::Optional)
    join(e.members, " ") * "?"
end

type ZeroOrMore <: _Compound
    members
    name

    function ZeroOrMore(members...; name="")
        new(collect(members), name)
    end
end

# Since length(node) is its number of children for iteration purposes
# call the length of the text of the node textlength
textlength(node::Node) = node._end - node.start + 1

# TODO: move to test_expressions.jl
@test textlength(Node("foo","",1,0)) == 0
@test textlength(Node("foo","f", 1,1)) == 1
@test textlength(Node("foo","foo", 1,3)) == 3

function _uncached_match(self::ZeroOrMore, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    new_pos = pos
    children = Node[]
    while true
        node = _match!(self.members[1], text, new_pos, cache, err)
        if isempty(node) || textlength(node) == 0
            return Node(self.name, text, pos, new_pos - 1, children)
        end
        push!(children, node)
        new_pos += textlength(node)
    end
    return EmptyNode()
end

function _as_rhs(e::Optional)
    join(e.members, " ") * "*"
end

type OneOrMore <: _Compound
    members
    name
    _min

    function OneOrMore(members...; name="", _min=1)
        new(collect(members), name, _min)
    end
end

function _uncached_match(self::OneOrMore, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    new_pos = pos
    children = Node[]
    while true
        node = _match!(self.members[1], text, new_pos, cache, err)
        if isempty(node)
            break
        end
        push!(children, node)
        len = textlength(node)
        if len == 0
            break
        end
        new_pos += len
    end
    if length(children) >= self._min
        return Node(self.name, text, pos, new_pos - 1, children)
    end
    return EmptyNode()
end

function _as_rhs(e::Optional)
    join(e.members, " ") * "+"
end

# Test
# TODO: move to test_expressions.jl

l = Literal("foo", name="foo")
println(l)
t = parse(l, "foo", 1)
println(t)
try
    println(parse(l, "foos", 1))
catch e
    println(e)
end
try
    println(parse(l, "bar", 1))
catch e
    println(e)
end
println(parse(Literal("foo"), "foo"))
println(parse(Regex("fo.*", options="", name="myregex"), "fooooo", 1))
println(parse(Sequence(l,l), "foofoo", 1))
println(parse(OneOf(Literal("foo"), Literal("bar")), "bar", 1))
println(parse(Sequence(Lookahead(Literal("fo")), Literal("foo")), "foo"))
println(parse(Sequence(Not(Literal("bar")), Literal("foo")), "foo"))
println(parse(Sequence(Optional(Literal("bar")), Literal("foo")), "foo"))
println(parse(Sequence(Optional(Literal("bar")), Literal("foo")), "barfoo"))
println(parse(ZeroOrMore(Literal("bar")), ""))
println(parse(ZeroOrMore(Literal("bar")), "bar"))
println(parse(ZeroOrMore(Literal("bar")), "barbar"))
println(parse(OneOrMore(Literal("bar")), "barbar"))
println(parse(OneOrMore(Literal("bar")), "bar"))
try
    println(parse(OneOrMore(Literal("bar")), "bas"))
catch e
    println(e)
end

end
