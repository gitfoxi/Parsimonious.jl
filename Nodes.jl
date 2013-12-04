
module Nodes

import Base: isequal, push!, length, start, next, done, match, show, print, showerror, isempty
export Node, NodeVisitor, isempty, nodetext, print, show, visit, visit_all, VisitationError, showerror

RegexMatchOrNothing = Union(RegexMatch, Nothing)

abstract AbstractNode

immutable Node{T}
    fulltext
    start
    _end
    children
    match

    function Node(fulltext, start, _end, children, match)
        @assert start > 0
        @assert _end <= length(fulltext)
        # _end < start is OK and indicates an empty match string
        new(fulltext, start, _end, children, match)
    end
end

immutable EmptyNode <: AbstractNode end

function show(io::IO, n::EmptyNode; indent=0)
    show(io, "EMPTYNODE")
end

function print(io::IO, n::Node)
    print(io, "<Node called '" * name(n) * "' matching '" * nodetext(n) *"'>")
end

function show(io::IO, n::Node; indent=0)
    print(io, "\n", repeat(".", indent))
    print(io, typeof(n), "\"")
    print(io, nodetext(n)[1:min(end,20)])
    if length(nodetext(n)) > 100
        print(io, "...", nodetext(n)[max(1,end-20):end])
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

function Node(T, fulltext, start::Int, _end::Int)
    Node{symbol(T)}(fulltext, start, _end, AbstractNode[], nothing)
end

function Node(T, fulltext, start::Int, _end::Int, children=AbstractNode[], match=nothing)
    Node{symbol(T)}(fulltext, start, _end, children, match)
end

# Copy
function Node(T, n::Node)
    Node(T, n.fulltext, n.start, n._end, n.children, n.match)
end

function Node()
    EmptyNode()
end

type VisitationError <:Exception
    node
    exc
    VisitationError(node, exc) = new(node, VisitationError)
end

VisitationError(node) = VisitationError(node, "VisitationError")

"""Something went wrong while traversing a parse tree.

This exception exists to augment an underlying exception with information
about where in the parse tree the error occurred. Otherwise, it could be
tiresome to figure out what went wrong; you'd have to play back the whole
tree traversal in your head.

"""
# TODO: Make sure this is pickleable. Probably use @property pattern. Make
# the original exc and node available on it if they don't cause a whole
# raft of stack frames to be retained.

function showerror(io::IO, e::VisitationError)
    print(io, "visitation error")
    print(io, "Exception: ")
    print(io, string(e.exc))
    print(io, "\nParse tree:\n")
    print(io, prettily(e.node, error=e.node))
end

function name(n::Node)
    s = string(typeof(n))
    @show s
    if search(s, ':') == 0
        return ""
    end
    s[search(s, ':')+1:end-1]
end

# must each node cary the same reference to fulltext? Probably.
# TODO: _end is the index of the last character in the end of the match.
# It used to be the character after that but I'm refactoring so lots to change.
function nodetext(n::Node)
#    @show n.start, n._end, typeof(n), length(n.fulltext)
# TODO: not sure escape goes here or elsewhere
    @show n.fulltext
    @show n.start
    @show n._end
    escape_string(n.fulltext[n.start:n._end])
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
    replace(s, r"\\n", "\\n")
end

function prettily{T}(node::Node{T}, err::AbstractNode=Node())
    ret = ["<$(T) matching '$(escape_newline(nodetext(node)))'>$(errstring(node, err))"]
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

# depth-first top-level visitation GO!
# Means you can't overload visit with less than 3 paramaeters without
# being very carefully. Maybe should give this it's own name like 'visit_all'
# Though I do appreciate the slickness of naming every function 'visit'
function visit{T}(v::NodeVisitor, node::Node{T})
    visited_children = [visit(v, n) for n in node]
    try
        return visit(v, node, visited_children)  # ...
    catch e
        @show e, v, node
        @show visited_children
        if isa(e, VisitationError)
            rethrow(e)
        else
            println("Throwing wrapped expression. Trying to anyway")
            @show node
            @show e
            rethrow(VisitationError(node, e))
        end
    end
end

# generic_visit -- not implemented in base class
function visit{T}(v::NodeVisitor, n::Node{T}, visited_children)
    error("Go implement visit(::$(typeof(v)), ::Node{$(T)})) right now!")
end

function lift_child(v::NodeVisitor, n::Node, child)
    @show "lift child", v, typeof(n), child
    return child
end

end
