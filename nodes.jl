
module Nodes

# export Node, NodeVisitor, isempty
using Base: Test
import Base: isequal, push!, length, start, next, done, match, show

RegexMatchOrNothing = Union(RegexMatch, Nothing)

abstract AbstractNode

immutable Node{T} <: AbstractNode
    fulltext
    start
    _end
    children
    match::RegexMatchOrNothing   # for Regex match objects

    function Node(fulltext::ASCIIString, start::Int, _end::Int, children::Array{AbstractNode,1}, match::RegexMatchOrNothing=nothing)
        # force me to implement a visitor for every possible thing because dropping to default is so unpredictable
        new {T}(fulltext, start, _end, children, match)
    end

end
n0 = Node{:myexpr}("beerbeerbeer", 5, 9, [], nothing)

immutable EmptyNode <: AbstractNode end

function show(io::IO, n::EmptyNode; indent=0)
    print(io, "EMPTYNODE")
end

function show(io::IO, n::Node; indent=0)
    print(io, "\n", repeat(".", indent))
    print(io, typeof(n), "\"")
    print(io, text(n)[1:min(end,20)])
    if length(text(n)) > 100
        print(io, "...", text(n)[max(1,end-20):end])
    end
    print(io, "\"")
    print(io, "(", n.start, ",", n._end, ")")
    if length(n.children) > 0
        print(io, "{")
        for child in n.children
            show(io, child, indent=indent+1)
            print(io, ",")
        end
        print(io, "}")
    end
end

# Here, I briefly wish I could have a union of two parametric types so I could
# make a RegexNode{T} with the extra match field

function Node(T::ASCIIString, fulltext::ASCIIString, start::Int, _end::Int)
    Node{symbol(T)}(fulltext, start, _end, AbstractNode[], nothing)
end
function Node(T::ASCIIString, fulltext::ASCIIString, start::Int, _end::Int; children=Node[], match=nothing)
    Node{symbol(T)}(fulltext, start, _end, children, match)
end

# Copy
function Node(T::ASCIIString, n::Node)
    Node(T, n.fulltext, n.start, n._end, n.children, n.match)
end

function Node()
    EmptyNode()
end

function name(n::Node)
    s = string(typeof(n))
    if search(s, ':') == 0
        return ""
    end
    s[search(s, ':')+1:end-1]
end

# must each node cary the same reference to fulltext? Probably.
# TODO: _end is the index of the next character after the end of the match. It
# should be the one before that but to change it a lot of things will have to
# change.
function text(n::Node)
#    @show n.start, n._end, typeof(n), length(n.fulltext)
# TODO: not sure escape goes here or elsewhere
    escape_string(n.fulltext[n.start:n._end - 1])
end

isempty(::Node) = false
isempty(::EmptyNode) = true
isequal(::EmptyNode, ::EmptyNode) = true

function isequal(a::Node, b::Node)
    # Can I make the type system do this?
    @show typeof(a) == typeof(b) 
    typeof(a) == typeof(b) || return false
    @show length(a) == length(b) 
    length(a) == length(b) || return false
    @show isequal(a.fulltext, b.fulltext) && a.start == b.start && a._end == b._end 
    @show a.fulltext
    @show b.fulltext
    isequal(a.fulltext, b.fulltext) && a.start == b.start && a._end == b._end || return false
    for i in 1:length(a)
        isequal(a.children[i], b.children[i]) || return false
    end
    true
end

function push!(node::AbstractNode, child::AbstractNode...)
    push!(node.children, child...)
end

function indent(s,indstr="| ")
    return join([indstr * line for line in split(s, '\n')], "\n")
end

function errstring(testnode, errnode)
    errnode === testnode ? " <-- *** Error here ***" : ""
end

function prettily(node::EmptyNode, err::AbstractNode=Node())
    return "<empty>$(errstring(node, err))"
end

function escape_newline(s)
    replace(s, r"""\n""", "\\n")
end

function prettily{T}(node::Node{T}, err::AbstractNode=Node())
    ret = ["<$(T) matching '$(escape_newline(text(node)))'>$(errstring(node, err))"]
    for child in node
        push!(ret, indent(prettily(child, err)))
    end
    return join(ret, "\n")
end

function pprettily(x...)
    println(prettily(x...))
end

# Thanks base/iterator.jl
# Iterate over Node children. I could turn this into the depth-first-search mechanism but that's not how Parsimonious does it.
length(n::Node) = length(n.children)
start(n::Node) = 1
function next(n::Node, state)
    n.children[state], state+1
end
done(n::Node, state) = state > length(n.children)

abstract NodeVisitor

# generic_visit
function visit{T}(v::NodeVisitor, n::Node{T})
    error("Go implement visit(::$(typeof(v)), ::Node{$(T)})) right now!")
end

function lift_child(v::NodeVisitor, n::Node, child)
    @show "lift child", v, typeof(n), child
    return child
end

# Test
mytext = "this is my text"
copytext = string(mytext)








n = Node("myexpr", mytext, 5, 9)
n2 = Node("myexpr", mytext, 5, 9)
n3 = Node("myexpr2", mytext, 5, 9)
nct = Node("myexpr", copytext, 5, 9)

@test length(n) == 0
@test isa(n, Node)
@test text(n) == " is "
@test !is(mytext, copytext)
@test isequal(n, nct)
@test isempty(Node())
@test !isempty(n)
@test isequal(n, n)
@test isequal(n, n2)
@test !isequal(n, n3)
@test isequal(Node(), Node())
push!(n2, n3)
push!(n2, Node())
push!(n2, n3, Node())
@test !isequal(n, n2)
@test length(n2.children) == 4
@test length(n2) == 4
@test errstring(n, n2) == ""
@test match(r"Error", errstring(n2, n2)) != nothing
@test match(r"Error", errstring(n, n2)) == nothing
@test indent(indent("foo","|"),"|") == "||foo"
@test indent("foo\nbar","  ") == "  foo\n  bar"

# TODO: test RegexNode
@show n2
for n in n2
    @show n
end
println(prettily(n2, n3))
println(prettily(n2, Node()))

type SomeVisitor <: NodeVisitor end
@test_throws visit(SomeVisitor(), n2)
# @test lift_child(SomeVisitor(), n2, "foo") == "foo"

# keyword syntax
nwc = Node("withchildren", mytext, 1, 4, children=[n n2 n3])
@test length(nwc.children) == 3
nwm = Node("withmatch", mytext, 1, 4, match=match(r"\S+", mytext))
@test nwm.match != nothing
nwcm = Node("with_match_and_children", mytext, 1, 4, match=match(r"\S+", mytext), children=[n n2 n3])
@test nwcm.match != nothing
@test length(nwcm.children) == 3

end
