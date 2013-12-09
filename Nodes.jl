
module Nodes

import Base: isequal, push!, length, start, next, done, match, show, print, showerror, isempty
export name, NodeText, length, _end, LeafNode, pprettily, RegexNode, AnyNode, MatchNode, ParentNode, ChildlessNode, Node, NodeVisitor, isempty, nodetext, print, show, visit, visit_all, VisitationError, showerror, push!, lift_child, EmptyNode, textlength

# I don't think EmptyNodes ever go in the tree. They are just used as a sort of
# error message. You return an EmptyNode when you fail to match, for example.

immutable ZeroLengthMatch
    pos::Int
end

immutable OneOrMoreMatch
    start::Int
    _end::Int

    function OneOrMoreMatch(start::Int, _end::Int)
        #@assert start > 0
        #@assert _end <= length(fulltext)
        #@assert _end >= start
        new(start, _end)
    end
end

Match = Union(ZeroLengthMatch, OneOrMoreMatch)

abstract MatchNode

immutable ParentNode{T} <: MatchNode
    match::Match
    children::Tuple

    # TODO: More specific type for children
    # Array{AbstractNode, 1} or Array{Node, 1} or Array{Node{:}} don't seem to work
    ParentNode(start::Int, _end::Int, children) = new(OneOrMoreMatch(start, _end), children)
    ParentNode(pos::Int, children) = new(ZeroLengthMatch(pos), children)
end

immutable ChildlessNode{T} <: MatchNode
    match::Match

    ChildlessNode(start::Int, _end::Int) = new(OneOrMoreMatch(start, _end))
    ChildlessNode(pos::Int) = new(ZeroLengthMatch(pos))
end

# Union does not serve my purposes
# Node = Union(ParentNode, EmptyNode, RegexNode, ChildlessNode)
# MatchNode = Union(ParentNode, ChildlessNode)
LeafNode = Union(ChildlessNode)  # because I might want to specialize leaf types ...
typealias EmptyNode Nothing
AnyNode = Union(EmptyNode, ParentNode, ChildlessNode)

# Convenience
## Parent
function Node(T::String, start::Int, _end::Int, children)
    if _end >= start
        ParentNode{symbol(T)}(start, _end, children)
    else
        ParentNode{symbol(T)}(start, children)
    end
end

## Child
function Node(T::String, start::Int, _end::Int)
    if _end >= start
        ChildlessNode{symbol(T)}(start, _end)
    else
        ChildlessNode{symbol(T)}(start)
    end
end

# Empty
Node() = nothing

# NodeText a tuple of Node and FullText
type NodeText
    node::MatchNode
    text::String
end

# Copy with new "name"
ParentNode(T, n::MatchNode) = ParentNode(T, n.match, n.children)
ChildlessNode(T, n::MatchNode) = ChildlessNode(T, n.match)

textlength(node::MatchNode) = length(node.match)
length(m::ZeroLengthMatch) = 0
length(m::OneOrMoreMatch) = m._end - m.start + 1

pos(m::ZeroLengthMatch) = m.pos
pos(m::OneOrMoreMatch) = m.start
range(m::ZeroLengthMatch) = m.pos:m.pos-1
range(m::OneOrMoreMatch) = m.start:m._end
text(nt::NodeText) = nt.text[range(nt.node.match)]

function goodnode_iprint(io::IO, n::MatchNode, t::String, indent::ASCIIString = "")
    # TODO: Right justify the text
    nodeinfo = "$(pos(n.match)) $(typeof(n))"
    nodetext = lpad("'$(text(NodeText(n,t)))'\n", 80 - length(indent) - length(nodeinfo))
    print(io, indent, nodeinfo, nodetext)
end

function iprint(io::IO, n::ParentNode, t::String, indent::ASCIIString = "")
    goodnode_iprint(io, n, t, indent)
    for c in n.children
        iprint(io, c, t, indent * "  .  ")
    end
end

function iprint(io::IO, n::ChildlessNode, t::String, indent::ASCIIString = "")
    goodnode_iprint(io, n, t, indent)
end

show(io::IO, nt::NodeText) = iprint(io, nt.node, nt.text)

type VisitationError <:Exception
    node
    fulltext
    exc
end

#"""Something went wrong while traversing a parse tree.
#
#This exception exists to augment an underlying exception with information
#about where in the parse tree the error occurred. Otherwise, it could be
#tiresome to figure out what went wrong; you'd have to play back the whole
#tree traversal in your head.
#
#"""
## TODO: Make sure this is pickleable. Probably use @property pattern. Make
## the original exc and node available on it if they don't cause a whole
## raft of stack frames to be retained.

function showerror(io::IO, e::VisitationError)
    println(io, "VISITATION ERROR")
    print(io, "Exception: ")
    # TODO: showerror
    print(io, string(e.exc))
    print(io, "\nParse tree:\n")
    print(io, prettily(e.node, e.fulltext, e.node))
end


## Really type-parameter but we're using that as the node's "name"
function name(n::AnyNode)
    s = string(typeof(n))
    if search(s, ':') == 0
        return ""
    end
    s[search(s, ':')+1:end-1]
end

 function nodetext(n::MatchNode, fulltext::String)
    text(NodeText(n,fulltext))
end

isempty(::EmptyNode) = true
isempty(::AnyNode) = false

function base_isequal(a::MatchNode, b::MatchNode)
    typeof(a) == typeof(b) || return false
    length(a) == length(b) || return false
    a.match == b.match || return false
    true
end

isequal(a::ChildlessNode, b::ChildlessNode) = base_isequal(a::MatchNode, b::MatchNode)
isequal(a::ParentNode, b::ChildlessNode) = false
isequal(a::ChildlessNode, b::ParentNode) = false

function isequal(a::ParentNode, b::ParentNode)
    base_isequal(a, b) || return false

    for (childa, childb) in zip(a.children, b.children)
        childa == childb || return false
    end
    true
end

# TODO: Can't work with children a tuple. delete ... have to make a new tuple if necessary
#function push!(node::ParentNode, child::MatchNode...)
#    push!(node.children, child...)
#end

function indent(s,indstr="| ")
    return join([indstr * line for line in split(s, '\n')], "\n")
end

## Make an error string if the testnode is the errnode
function errstring(testnode, errnode)
    errnode === testnode ? " <-- *** Error here ***" : ""
end

# I don't think empty nodes go in tree ... maybe delete this:
function prettily(node::EmptyNode, fulltext, errnode::AnyNode=EmptyNode())
    return "<empty>$(errstring(node, errnode))"
end

# function prettily{T}(node::Node{T}, err::Node=Node())
function prettily{T}(node::ParentNode{T}, fulltext, errnode::AnyNode=EmptyNode())
    ret = ["<$(T) matching '$(escape_string(text(NodeText(node, fulltext))))'>$(errstring(node, errnode))"]
    for child in node
        push!(ret, indent(prettily(child, fulltext, errnode)))
    end
    return join(ret, "\n")
end

function prettily{T}(node::ChildlessNode{T}, fulltext, errnode::AnyNode=EmptyNode())
    "<$(T) matching '$(escape_string(text(NodeText(node,fulltext))))'>$(errstring(node, errnode))"
end

# TODO merge functions prettily and show which do very similar things
function pprettily(x...)
    println(prettily(x...))
end

## Thanks base/iterator.jl
## Iterate over Node children. I could turn this into the depth-first-search mechanism but that's not how Parsimonious does it.
length(n::ParentNode) = length(n.children)
start(n::ParentNode) = 1
function next(n::ParentNode, state)
    n.children[state], state+1
end
done(n::ParentNode, state) = state > length(n.children)

length(::EmptyNode) = 0
length(::LeafNode) = 0  # Childless

#
abstract NodeVisitor
#
## depth-first top-level visitation GO!
## Means you can't overload visit with less than 3 paramaeters without
## being very carefully. Maybe should give this it's own name like 'visit_all'
## Though I do appreciate the slickness of naming every function 'visit'
#
## Currently handled in Grammars but TODO: move back here
# Refactor: Pass around fulltext with every call. Seems fun.
# TODO: For child nodes

function visit_on_the_way_down(v::NodeVisitor, fulltext::String, node::ParentNode)
    return [visit(v, fulltext, n) for n in node]
end

# No children
visit_on_the_way_down(v::NodeVisitor, fulltext::String, node::LeafNode) = []

function visit(v::NodeVisitor, fulltext::String, node::MatchNode)
    visited_children = visit_on_the_way_down(v, fulltext, node)
    try
        # TODO: Move this whole thing to tuples -- but not now
        return visit(v, fulltext, node, visited_children)  # ...
    catch e
        if isa(e, VisitationError)
            rethrow(e)
        else
            rethrow(VisitationError(node, fulltext, e))
        end
    end
end

## generic_visit -- not implemented in base class as it were
function visit{T}(v::NodeVisitor, n::ParentNode{T}, visited_children)
    # TODO: backtrace broken
    # TODO: backtrace works on Linux, broken on OSX only?
    println("Go implement visit(::$(string(typeof(v))), ::Node{$(name(n))})) right now!")
    error("Go implement visit(::$(string(typeof(v))), ::Node{$(name(n))})) right now!")
end

## conveniently replace a node with its own visited_children
function lift_child(v::NodeVisitor, f::String, n::ParentNode, visited_children)
    return visited_children[1]
end

# Refactoring convenience
# TODO: test or remove
_end(m::OneOrMoreMatch) = m._end
_end(m::ZeroLengthMatch) = m.pos - 1
_end(n::MatchNode) = _end(n.match)
Node(T::String, ::String, start::Int, _end::Int, children::Tuple) = Node(T, start, _end, children)
Node(T::String, ::String, start::Int, _end::Int, children::Array) = Node(T, start, _end, tuple(children...))
Node(T::String, ::String, start::Int, _end::Int) = Node(T, start, _end)
end
