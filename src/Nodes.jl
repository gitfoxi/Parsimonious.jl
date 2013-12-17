
import Base: isequal, push!, length, start, next, done, match, show, print, showerror, isempty
export LeafNode, name, length, _end, LeafNode, pprettily, AnyNode, MatchNode, ParentNode, Node, NodeVisitor, isempty, nodetext, print, show, visit, VisitationError, showerror, push!, lift_child, EmptyNode, textlength, pos

# I don't think EmptyNodes ever go in the tree. They are just used as a sort of
# error message. You return an EmptyNode when you fail to match, for example.

abstract MatchNode

immutable ParentNode{T} <: MatchNode
    match::SubString
    children::Tuple

    # TODO: More specific type for children
    # Array{AbstractNode, 1} or Array{Node, 1} or Array{Node{:}} don't seem to work
end

immutable LeafNode{T} <: MatchNode
    match::SubString
end

# Union does not serve my purposes
# Node = Union(ParentNode, EmptyNode, RegexNode, ChildlessNode)
# MatchNode = Union(ParentNode, ChildlessNode)
typealias EmptyNode LeafNode{:ifeelsoempty}
AnyNode = Union(EmptyNode, ParentNode, LeafNode)

# Convenience
## Parent
function Node(T::String, match::SubString, children)
    ParentNode{symbol(T)}(match, children)
end

## Leaf
function Node(T::String, match::SubString)
    LeafNode{symbol(T)}(match)
end

# Empty
EmptyNode() = LeafNode{:ifeelsoempty}(SubString("", 1))
Node() = EmptyNode()

function goodnode_iprint(io::IO, n::MatchNode, indent::ASCIIString = "")
    # TODO: Right justify the text
    nodeinfo = "$(pos(n)) $(typeof(n))"
    nodetext = lpad("'$(n.match)'\n", 80 - length(indent) - length(nodeinfo))
    print(io, indent, nodeinfo, nodetext)
end

function iprint(io::IO, n::ParentNode, indent::ASCIIString = "")
    goodnode_iprint(io, n, indent)
    for c in n.children
        iprint(io, c, indent * "  .  ")
    end
end

function iprint(io::IO, n::LeafNode, indent::ASCIIString = "")
    goodnode_iprint(io, n, indent)
end

show(io::IO, n::MatchNode) = iprint(io, n)

type VisitationError <:Exception
    node
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
    print(io, prettily(e.node, e.node))
end


## Really type-parameter but we're using that as the node's "name"
function name(n::AnyNode)
    s = string(typeof(n))
    if search(s, ':') == 0
        return ""
    end
    s[search(s, ':')+1:end-1]
end

isempty(::LeafNode{:ifeelsoempty}) = true
isempty(::AnyNode) = false

function base_isequal(a::MatchNode, b::MatchNode)
    typeof(a) == typeof(b) || return false
    length(a) == length(b) || return false
    a.match == b.match || return false
    true
end

isequal(a::LeafNode, b::LeafNode) = base_isequal(a::MatchNode, b::MatchNode)
isequal(a::ParentNode, b::LeafNode) = false
isequal(a::LeafNode, b::ParentNode) = false

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
function prettily(node::EmptyNode, errnode::AnyNode=EmptyNode())
    return "<empty>$(errstring(node, errnode))"
end

# function prettily{T}(node::Node{T}, err::Node=Node())
function prettily{T}(node::ParentNode{T}, errnode::AnyNode=EmptyNode())
    ret = ["<'$(T)' matching '$(escape_string(node.match))'>$(errstring(node, errnode))"]
    for child in node
        push!(ret, indent(prettily(child, errnode)))
    end
    return join(ret, "\n")
end

function prettily{T}(node::LeafNode{T}, errnode::AnyNode=EmptyNode())
    "<'$(T)' matching '$(escape_string(node.match))'>$(errstring(node, errnode))"
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
length(::LeafNode) = 0  # LeafNode

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

function visit_on_the_way_down(v::NodeVisitor, node::ParentNode)
    return [visit(v, n) for n in node]
end

# No children
visit_on_the_way_down(v::NodeVisitor, node::LeafNode) = []

function visit(v::NodeVisitor, node::MatchNode)
    visited_children = visit_on_the_way_down(v, node)
    try
        # TODO: Move this whole thing to tuples -- but not now
        # TODO: uncomment for debug mode
#        @show v
#        @show name(node)
#        @show visited_children
#        @which visit(v, node, visited_children)
        returns = visit(v, node, visited_children)
#        @show returns
        return returns
    catch e
        if isa(e, VisitationError)
            rethrow(e)
        else
            rethrow(VisitationError(node, e))
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
function lift_child(v::NodeVisitor, n::ParentNode, visited_children)
    return visited_children[1]
end

# Refactoring convenience
# TODO: test or remove
import Base.chr2ind

Node(T::String, fulltext::String, start::Int, endind::Int, children::Tuple) = Node(T, SubString(fulltext, start, endind), children)
Node(T::String, fulltext::String, start::Int, endind::Int) = Node(T, SubString(fulltext, start, endind))
_end(n::MatchNode) = n.match.endof == 0 ? prevind(n.match.string, pos(n) - 1) : n.match.offset + n.match.endof
pos(n::MatchNode) = n.match.offset + (n.match.endof == 0 ? 0 : 1)
# refactor pos -> ind since we deal in byte indecies now
# refactor _end
nodetext(n::MatchNode) = n.match

textlength(node::MatchNode) = endof(node.match)

