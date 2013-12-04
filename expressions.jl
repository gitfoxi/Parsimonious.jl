
# TODO:  _as_rhs

module Expressions

export Regex

import Base: match
import Nodes: Node, isempty

abstract Expression # -ism

type ParseError
    text::ASCIIString
    expr::Expression
    pos::Int64
end

function parse(expr::Expression, text::ASCIIString, pos::Int64=1)
    node = match(expr, text, pos)
    if node._end < length(text) + 1
        error("Incomplete parse, stopped at char $(node._end) of $(pos): $(text[node._end:min(end, node._end+100)])")
    end
    node
end

function match(expr::Expression, text::ASCIIString, pos::Int64=1)
    err = ParseError(text, expr, pos)
    node = _match!(expr, text, pos, Dict(), err)
    if isempty(node)
        println("err.text: ", err.text)
        println("expr: ", expr)
        error(err.text)
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
        return Node(literal.name, text, pos, pos + length(literal.literal))
    end
    Node()  # "Empty" node
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
        return Node()
    end
    assert(m.offset == 1)  # should only match start of text
    Node(regex.name, text, pos, pos + length(m.match), match=m)
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
        length = node._end - node.start
        new_pos += length
        length_of_sequence += length
    end
    # Hooray! We got through all the members!
    return Node(sequence.name, text, pos, pos + length_of_sequence, children=children)
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
            return Node(oneof.name, text, pos, node._end, children=[node])
        end
    end
    return Node()
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
        return Node(self.name, text, pos, pos)
    end
    return Node()
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
        return Node(self.name, text, pos, pos)
    end
    return Node()
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
        return Node(self.name, text, pos, pos)
    end
    return Node(self.name, text, pos, node._end, children=[node])
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

function _uncached_match(self::ZeroOrMore, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    new_pos = pos
    children = Node[]
    while true
        node = _match!(self.members[1], text, new_pos, cache, err)
        if isempty(node) || (node._end - node.start) == 0
            return Node(self.name, text, pos, new_pos, children=children)
        end
        push!(children, node)
        new_pos += node._end - node.start
    end
    return Node()
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
        len = node._end - node.start
        if len == 0
            break
        end
        new_pos += len
    end
    if length(children) >= self._min
        return Node(self.name, text, pos, new_pos, children=children)
    end
    return Node()
end

function _as_rhs(e::Optional)
    join(e.members, " ") * "+"
end

# Test

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
