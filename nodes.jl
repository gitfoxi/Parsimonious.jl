
module Nodes

# export Node, NodeVisitor, isempty
using Base: Test
import Base: isequal, push!, length, start, next, done, match

abstract AbstractNode
RegexMatchOrNothing = Union(RegexMatch, Nothing)
immutable Node{T} <: AbstractNode
    fulltext
    start
    _end
    children
    match::RegexMatchOrNothing   # for Regex match objects
end

# Here, I briefly wish I could have a union of two parametric types so I could
# make a RegexNode{T} with the extra match field

immutable EmptyNode <: AbstractNode end

function Node(T::ASCIIString, fulltext, start, _end; children=AbstractNode[], match=nothing)
    Node{symbol(T)}(fulltext, start, _end, children, match)
end

function Node(T::ASCIIString, n::Node)
    Node{symbol(T)}(n.fulltext, n.start, n._end, n.children, n.match)
end

function Node()
    EmptyNode()
end

# must each node cary the same reference to fulltext? Probably.
# TODO: _end is the index of the next character after the end of the match. It
# should be the one before that but to change it a lot of things will have to
# change.
function text(n::Node)
#    @show n.start, n._end, typeof(n), length(n.fulltext)
    n.fulltext[n.start:n._end - 1]
end

isempty(::Node) = false
isempty(::EmptyNode) = true
isequal(::EmptyNode, ::EmptyNode) = true

function isequal(a::Node, b::Node)
    # Can I make the type system do this?
    typeof(a) == typeof(b) || return false
    length(a) == length(b) || return false
    a.children == b.children || return false
    is(a.fulltext, b.fulltext) && a.start == b.start && a._end == b._end || return false
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
#type ConcreteVisitor <: NodeVisitor
#    state
#end

# generic_visit
function visit{T}(v::NodeVisitor, n::Node{T})
    error("Go implement visit(::$(typeof(v)), ::Node{$(T)})) right now!")
end

# TODO: Not sure what this is for so it's probably not right
function lift_child(v::NodeVisitor, n::Node)
    # convience method to replace this node with it's first child
    n.children[1]
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
@test !isequal(n, nct)
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
@test lift_child(SomeVisitor(), n2) == n3

# keyword syntax
nwc = Node("withchildren", mytext, 1, 4, children=[n n2 n3])
@test length(nwc.children) == 3
nwm = Node("withmatch", mytext, 1, 4, match=match(r"\S+", mytext))
@test nwm.match != nothing
nwcm = Node("with_match_and_children", mytext, 1, 4, match=match(r"\S+", mytext), children=[n n2 n3])
@test nwcm.match != nothing
@test length(nwcm.children) == 3

end
