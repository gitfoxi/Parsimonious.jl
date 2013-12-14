
module Expressions

import Base: match, rsearch, length, showerror, isequal, show
using Nodes
using Util
export _Compound, LazyReference, unicode, IncompleteParseError, showerror, ParseError, Expression, Literal, Regex, Sequence, OneOf, Not, Optional, ZeroOrMore, OneOrMore, parse, Lookahead, isequal, @p_str, @p_mstr, show, as_rule, match

abstract Expression # -ism
abstract ParseException

type ParseError <: ParseException
    text::String
    expr::Expression
    pos::Int
end

# TODO: DRY
# A future Julia feature will be abstract types with fields
type IncompleteParseError <: ParseException
    text::String
    expr::Expression
    pos::Int
end

function showerror(io::IO, e::ParseError)
    print_escaped(io, "Rule '$(e.expr.name)' didn't match at '$(e.text[e.pos:min(end,e.pos+20)])' (line $(line(e)), column $(column(e))).", "")
end

function unicode(e::ParseException)
    io = IOString()
    showerror(io, e)
    seekstart(io)
    readline(io)
end

function showerror(io::IO, e::IncompleteParseError)
    print_escaped(io, "Rule '$(e.expr.name)' matched in its entirety, but it didn't consume all the text. The non-matching portion of the text begins with '$(e.text[e.pos+1:min(end,e.pos + 20)])' (line $(line(e)), column $(column(e))).", "")
end

line(e::ParseException) = 1 + count(e.text[1:e.pos-1]) do c c == '\n' end

column(e::ParseException) = e.pos - rsearch(e.text, '\n', e.pos - 1)

function parse(expr::Expression, text::String, pos::Int=1)
    node = match(expr, text, pos)
    if _end(node) != length(text)
        throw(IncompleteParseError(text, expr, _end(node)))
    end
    node
end

function match(expr::Expression, text::String, pos::Int=1)
    err::ParseError = ParseError(text, expr, pos)
    cache = Dict{Uint, AnyNode}()
    sizehint(cache, 10*length(text))
    node = _match(expr, text, pos, cache, err)
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

function _match(expr::Expression, text::String, pos::Int, cache::Dict, err::ParseError)
    # This is interesting. Tuple keys are slow so
    #   key = (object_id(expr), pos)
    # results in a much slower program
    # This key, however, is subject to collision, however unlikely so is a bug in the now much-faster program
    # When tuple keys are fixed, go back to the proper key.
    key::Uint = (uint(object_id(expr) << 30) | pos)
    local node
    if !haskey(cache, key)
        # TODO: hottest of all hot spots here
        #@show expr.name, typeof(expr), pos
        #println(text[pos:min(pos + 20, end)])
        node = cache[key] = _uncached_match(expr, text, pos, cache, err)

        #@show node
        #if isa(node, MatchNode)
        #    @show nodetext(node, text)
        #end
        # For fun, to see memory savings replace above line with:
        # node = _uncached_match(expr, text, pos, cache, err)
        # Surprisingly, memory usage went up to 183M without the cache from 139M with.
        # Either way that is a lot of fucking memory allocated to parse a few kB of text.
        # Hopefully @time reports it some wierd way and it's not really chewing up so
        # much memory.
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
    literal::String
    name::String
    length::Int

    Literal(literal, name) = new(literal, name, length(literal))
end

# TODO: get rid of all keyword default constructors because slow
# Also, try not to use keyword constructors in Grammars.jl
Literal(literal::String; name::String="") = Literal(literal, name)

function _uncached_match(literal::Literal, text::String, pos::Int, cache::Dict, err::ParseError)
    if beginswith(SubString(text, pos, length(text)), literal.literal)
        return Node(literal.name, text, pos, pos - 1 + literal.length)
    end
    EmptyNode()  # "Empty" node == no match
end

function _as_rhs(literal::Literal)
    "\"" * literal.literal * "\""
end

# Stefan won't accept my anchor-regex patch. Fine
function MyBaseRegex(pattern::String, flags::String)
    options = Base.DEFAULT_OPTS
    PCRE = Base.PCRE
    for f in flags
        options |= f=='i' ? PCRE.CASELESS  :
                   f=='m' ? PCRE.MULTILINE :
                   f=='s' ? PCRE.DOTALL    :
                   f=='x' ? PCRE.EXTENDED  :
                   f=='a' ? PCRE.ANCHORED  :
                   error("unknown regex flag: $f")
    end
    Base.Regex(pattern, options)
end

type Regex <: Expression
    name::String
    re::Base.Regex
    options
    original_re

    function Regex(pattern, name="", options="")
        original_re = pattern
        options *= "a"
        @assert(length(pattern) > 0)

        new(name, MyBaseRegex(pattern, options), options, original_re)
    end
end

Regex(pattern::String; name="", options="") = Regex(pattern, name, options)

# This is really something magical. Say you want an unescaped string, just say:
#    s = p"\'"
# And you will have a 2 character string where the characters are '\' and '''.
# How does the macro do it? I don't know. Thanks `regex.jl`

function _uncached_match(regex::Regex, text::String, pos::Int, cache::Dict, err::ParseError)
# TODO: want but can't because bug
    m = Base.match(regex.re, text, pos)
    if m == nothing
        return EmptyNode()
    end
    Node(regex.name, text, pos, pos - 1 + length(m.match)) # no longer special , m)
end

function _as_rhs(regex::Regex)
    "~\"$(regex.original_re)\"$(regex.options)"
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
# special handling for the re field
        if field == :re
            a.re.pattern == b.re.pattern || return false
        else
            getfield(a, field) == getfield(b, field) || return false
        end
    end
    true
end

using Base.Test
function Regex(pattern::Base.Regex; name="", options="")
# If someone uses a regex string, unpack it and use it anyway
    Regex(pattern.pattern, name, options)
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

function _uncached_match(sequence::Sequence, text::String, pos::Int, cache::Dict, err::ParseError)
    new_pos = pos
    length_of_sequence = 0
    first = true
    local children
    for m in sequence.members
        node = _match(m, text, new_pos, cache, err)
        if isempty(node)
            return node
        end
        if first
            children = MatchNode[node]
            first = false
        else
            push!(children, node)
        end
        new_pos += textlength(node)
        length_of_sequence += textlength(node)
    end
    # Hooray! We got through all the members!
    return Node(sequence.name, text, pos, pos + length_of_sequence - 1, tuple(children...))
end

function _as_rhs(sequence::Sequence)
    return join([m.name for m in sequence.members], " ")
end

type OneOf <: _Compound
    name::String
    members # ::(Expression...) -- crashes resolve_refs

    OneOf(name::String, members::Expression...) = new(name,members)
    OneOf(members::Expression...) = new("", members)
end

OneOf(members::Expression...; name::String="") = OneOf(name, members...)

function _uncached_match(oneof::OneOf, text::String, pos::Int, cache::Dict, err::ParseError)
    local members = oneof.members
    for (i, m) in enumerate(members)
        node = _match(m, text, pos, cache, err)
        if !isempty(node)
            return Node(oneof.name, text, pos, _end(node), (node,))
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

function _uncached_match(self::Lookahead, text::String, pos::Int, cache::Dict, err::ParseError)
    node = _match(self.members[1], text, pos, cache, err)
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

function _uncached_match(self::Not, text::String, pos::Int, cache::Dict, err::ParseError)
    node = _match(self.members[1], text, pos, cache, err)
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

function _uncached_match(self::Optional, text::String, pos::Int, cache::Dict, err::ParseError)
    node = _match(self.members[1], text, pos, cache, err)
    if isempty(node)
        return Node(self.name, text, pos, pos - 1)
    end
    return Node(self.name, text, pos, _end(node), (node,))
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

function _uncached_match(self::ZeroOrMore, text::String, pos::Int, cache::Dict, err::ParseError)
    new_pos = pos
    first = true
    local children
    while true
        node = _match(self.members[1], text, new_pos, cache, err)
        if isempty(node) || textlength(node) == 0
            first && return Node(self.name, text, pos, new_pos - 1)
            return Node(self.name, text, pos, new_pos - 1, tuple(children...))
        end
        if first
            children = MatchNode[node]
            first = false
        else
            push!(children, node)
        end
        new_pos += textlength(node)
    end
    return EmptyNode()
end

function _as_rhs(e::ZeroOrMore)
    join(e.members, " ") * "*"
end

type OneOrMore <: _Compound
    name::String
    _min::Int
    members

    # TODO: too many constructors -- members should always be first because it's the only thing that is always there
    OneOrMore(name::String="", _min::Int=1, members::Expression...=Expression[]) = new(name, _min, members)
    OneOrMore(members::Expression...=Expression[], name::String="", _min::Int=1) = new(name, _min, members)
end

OneOrMore(members::Expression...; _min::Int=1, name::String="") = OneOrMore(name, _min, members...)

function _uncached_match(self::OneOrMore, text::String, pos::Int, cache::Dict, err::ParseError)
    new_pos = pos
    first = true
    local children
    while true
        node = _match(self.members[1], text, new_pos, cache, err)
        if isempty(node)
            break
        end
        if first
            children = MatchNode[node]
            first = false
        else
            push!(children, node)
        end
        len = textlength(node)
        if len == 0
            break
        end
        new_pos += len
    end
    if !first && length(children) >= self._min
        return Node(self.name, text, pos, new_pos - 1, tuple(children...))
    end
    return EmptyNode()
end

type LazyReference <: Expression 
    name
    label
end

_as_rhs(lr::LazyReference) = "<LazyReference to $(lr.label)>"

unicode(lr::LazyReference) = lr.name

function _as_rhs(e::OneOrMore)
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

indent(stack) = "  .  "^length(stack)
exprid(expr) = string(typeof(expr)) * " <" * expr.name * ">"
function show(io::IO, expr::Expression)
    function reshow(io::IO, expr::Expression, stack::Set)
        println(io, indent(stack), exprid(expr), ": ")
        for field in sort(names(expr))
            if(field == :name)
# already taken care of
            elseif(field == :members)
                push!(stack, exprid(expr))
                for m in expr.members
                    if in(exprid(m), stack)
                        println(io, indent(stack), "RECURSIVE " * exprid(expr) * " ...")
                    else
                        reshow(io, m, stack)
                    end
                end
                delete!(stack, exprid(expr))
            else
                println(io, indent(stack), ' ', string(field), '(', getfield(expr, field), ')')
            end
        end
    end
    reshow(io, expr, Set())
end

end
