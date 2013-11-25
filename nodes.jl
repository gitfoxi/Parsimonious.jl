

# require("Stuff")
module Nodes

export Node, NodeVisitor, isempty

type Node
    exprname
    fulltext
    start
    _end
    children
end

Node(exprname, fulltext, start, _end) = Node(exprname, fulltext, start, _end, [])
Node() = Node("", "", -1, -1)


function text(n::Node)
    n.fulltext[n.start:n._end]
end

function isempty(node::Node)
    return node.start == -1
end

abstract NodeVisitor

# Test
n = Node("myexpr", "longlongtext", 5, 8)
println(n)
println(text(n))
println(isempty(Node()))

end
