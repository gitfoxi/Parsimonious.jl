
module Expressions

import Base: match, rsearch, length, showerror, isequal
using Nodes
export unicode, IncompleteParseError, showerror, ParseError, Expression, Literal, Regex, Sequence, OneOf, Not, Optional, ZeroOrMore, OneOrMore, parse, Lookahead, isequal

abstract Expression # -ism
abstract ParseException

type ParseError <: ParseException
    text::ASCIIString
    expr::Expression
    pos::Integer
end

# TODO: DRY
# A future Julia feature will be abstract types with fields
type IncompleteParseError <: ParseException
    text::ASCIIString
    expr::Expression
    pos::Integer
end

function showerror(io::IO, e::ParseError)
    print(io, escape_string("Rule '$(e.expr.name)' didn't match at '$(e.text[e.pos:min(end,e.pos+20)])' (line $(line(e)), column $(column(e)))."))
end

function unicode(e::ParseException)
    io = IOString()
    showerror(io, e)
    seekstart(io)
    readline(io)
end

function showerror(io::IO, e::IncompleteParseError)
    print(io, escape_string("Rule $(e.expr.name) matched in its entirety, but it didn't consume all the text. The non-matching portion of the text begins with '$(e.text[e.pos:min(end,e.pos + 20)])' (line $(line(e)), column $(column(e)))."))
end

line(e::ParseException) = 1 + count(e.text[1:e.pos-1]) do c c == '\n' end

column(e::ParseException) = e.pos - rsearch(e.text, '\n', e.pos - 1)

function parse(expr::Expression, text::ASCIIString, pos::Int64=1)
    node = match(expr, text, pos)
    if node._end != length(text)
        throw(IncompleteParseError(text, expr, node._end))
    end
    node
end

function match(expr::Expression, text::ASCIIString, pos::Int64=1)
    err = ParseError(text, expr, pos)
    node = _match!(expr, text, pos, Dict(), err)
    if isempty(node)
        throw(err)
    end
    node
end

function hasfield(t, field)
    try
        getfield(t, field)
    catch
        return false
    end
    true
end

function _match!(expr::Expression, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    expr_id = object_id(expr)
    key = (expr_id, pos)
    if !haskey(cache, key)
        node = cache[key] = _uncached_match(expr, text, pos, cache, err)
        cache[key] = node
    else
        node = cache[key]
    end

    if isempty(node) && pos >= err.pos && (
        !isempty(expr.name) || !hasfield(err.expr, :name))
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
end

# TODO: get rid of all keyword default constructors because slow
# Also, try not to use keyword constructors in Grammars.jl
Literal(literal::ASCIIString; name::ASCIIString="") = Literal(literal, name)

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

    function Regex(pattern, name="", options="")
        @assert(length(pattern) > 0)
        # Hat '^' enforces match from beginning of pattern
        if pattern[1] != '^'
            pattern = "^" * pattern
        end

        new(name, Base.Regex(pattern, options))
    end
end

Regex(pattern; name="", options="") = Regex(pattern, name, options)

function _uncached_match(regex::Regex, text::ASCIIString, pos::Int64, cache::Dict, err::ParseError)
    m = match(regex.re, text[pos:])
    if isa(m, Nothing)
        return EmptyNode()
    end
    assert(m.offset == 1)  # should only match start of text
    println("REGEX MATCHED ", regex.re.pattern, " MATCH ", m.match, "NAME", regex.name, "LENGTH", length(m.match))
    n = Node(regex.name, text, pos, pos - 1 + length(m.match), match=m)
    @show nodetext(n)
    n
end

function _as_rhs(regex::Regex)
    "~\"$(re.pattern)\"$(re.options)"
end

abstract _Compound <: Expression
# has members

# A bit convoluted. You can efficiently create:
#   Sequence(name, members...)
#   Sequence(members...)  # no name
# And less-efficiently
#   Sequence(members..., name="name")
type Sequence <: _Compound
    name
    members

    Sequence(name::String, members::Expression...) = new(name,members)
    Sequence(members::Expression...) = new("", members)
end

# TODO: DRY: generic constructor for _Compound things?
Sequence(members::Expression...; name::String="") = Sequence(name, members...)

function isequal(a::Expression, b::Expression)
    typeof(a) == typeof(b) || return false
    for field in names(a)
        getfield(a, field) == getfield(b, field) || return false
    end
    true
end

# TODO: Why can't isequal just work for any two same-type objects?
# TODO: ask julia-users
function isequal(a::_Compound, b::_Compound)
    typeof(a) == typeof(b) || return false
    length(a.members) == length(b.members) || return false
    for (i, j) in zip(a.members, b.members)
        i == j || return false
    end
    for field in names(a)
        if field != :members
            getfield(a, field) == getfield(b, field) || return false
        end
    end
    true
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
    name
    members

    OneOf(name::String, members::Expression...) = new(name,members)
    OneOf(members::Expression...) = new("", members)
end

OneOf(members::Expression...; name::String="") = OneOf(name, members...)

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
    name
    members

    Lookahead(name::String, members::Expression...) = new(name,members)
    Lookahead(members::Expression...) = new("", members)
end

Lookahead(members::Expression...; name::String="") = Lookahead(name, members...)

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
    name
    members

    Not(name::String, members::Expression...) = new(name,members)
    Not(members::Expression...) = new("", members)
end

Not(members::Expression...; name::String="") = Not(name, members...)

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
    name
    members

    Optional(name::String, members::Expression...) = new(name,members)
    Optional(members::Expression...) = new("", members)
end

Optional(members::Expression...; name::String="") = Optional(name, members...)

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
    name
    members

    ZeroOrMore(name::String, members::Expression...) = new(name,members)
    ZeroOrMore(members::Expression...) = new("", members)
end

ZeroOrMore(members::Expression...; name::String="") = ZeroOrMore(name, members...)

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
    name
    _min
    members

    # TODO: too many constructors -- members should always be first because it's the only thing that is always there
    OneOrMore(name::String="", _min::Integer=1, members::Expression...=Expression[]) = new(name, _min, members)
    OneOrMore(members::Expression...=Expression[], name::String="", _min::Integer=1) = new(name, _min, members)
end

OneOrMore(members::Expression...; _min::Integer=1, name::String="") = OneOrMore(name, _min, members...)

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

type LazyReference <: Expression name end

_as_rhs(lr::LazyReference) = "<LazyReference to $(lr.name)>"

unicode(lr::LazyReference) = lr.name

function _as_rhs(e::Optional)
    join(e.members, " ") * "+"
end

function as_rule(expr::Expression)
    if expr.name == ""
        return _as_rhs(expr)
    end

    "$(expr.name) = $(_as_rhs(expr))"
end

function unicode(expr::Expression)
    class = string(typeof(expr))
    rulestr = as_rule(expr)
    id = hex(object_id(expr))
    "<$(class) $(rulestr) at 0x$(id)>"
end

end
