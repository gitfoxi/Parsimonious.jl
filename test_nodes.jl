# -*- coding: utf-8 -*-

# TODO: refactor 'text()' function as 'nodetext()'

# from nose.tools import eq_, assert_raises
# 
# from parsimonious.nodes import Node, NodeVisitor, VisitationError
 
using Base.Test
# import Nodes: Node, NodeVisitor, VisitationError, string, repr, visit
reload("Nodes.jl")  # for repl debugging
using Nodes
import Nodes.visit  # for overloading
import Nodes: errstring # for testing internal stuff

type HtmlFormatter <: NodeVisitor end

# 
# class HtmlFormatter(NodeVisitor):
#     """Visitor that turns a parse tree into HTML fragments"""
# 
#     def visit_bold_open(self, node, visited_children):
#         return '<b>'

visit(::HtmlFormatter, ::Node{:bold_open}, _) = "<b>"

#     def visit_bold_close(self, node, visited_children):
#         return '</b>'
# 

visit(::HtmlFormatter, ::Node{:bold_close}, _) = "</b>"

#     def visit_text(self, node, visited_children):
#         """Return the text verbatim."""
#         return node.text

visit(::HtmlFormatter, node::Node{:text}, _) = nodetext(node)

# 
#     def visit_bold_text(self, node, visited_children):
#         return ''.join(visited_children)

visit(::HtmlFormatter, ::Node{:bold_text}, visited_children) = join(visited_children, "")

type ExplosiveFormatter <: NodeVisitor end

# 
# 
# class ExplosiveFormatter(NodeVisitor):
#     """Visitor which raises exceptions"""
# 
#     def visit_boom(self, node, visited_children):
#         raise ValueError

# BoundsError is supposed to get wrapped with VisitationError
visit(::ExplosiveFormatter, ::Node{:boom}, _) = throw(BoundsError)

# 
# 
# def test_visitor():
#     """Assert a tree gets visited correctly.
# 
#     We start with a tree from applying this grammar... ::
# 
#         bold_text  = bold_open text bold_close
#         text       = ~'[a-zA-Z 0-9]*'
#         bold_open  = '(('
#         bold_close = '))'
# 
#     ...to this text::
# 
#         ((o hai))
# 
#     """
#     text = '((o hai))'
#     tree = Node('bold_text', text, 0, 9,
#                 [Node('bold_open', text, 0, 2),
#                  Node('text', text, 2, 7),
#                  Node('bold_close', text, 7, 9)])
#     result = HtmlFormatter().visit(tree)
#     eq_(result, '<b>o hai</b>')

text = "((o hai))"
tree = Node("bold_text", text, 1, 9,
            [Node("bold_open", text, 1, 2),
              Node("text", text, 3, 7),
              Node("bold_close", text, 8, 9)])
result = visit(HtmlFormatter(), tree)
@show result
@test result == "<b>o hai</b>"


# def test_visitation_exception():
#     assert_raises(VisitationError,
#                   ExplosiveFormatter().visit,
#                   Node('boom', '', 0, 0))

n = Node("boom", "", 1, 0)
@test_throws visit(ExplosiveFormatter(), n)
try
    visit(ExplosiveFormatter(), n)
catch ex
    if isa(ex, VisitationError)
        @test true
    else
        println("Rethrowing in test_nodes.jl which is bad")
        rethrow(ex)
    end
end

# 
# 
# def test_str():
#     """Test str and unicode of ``Node``."""
#     n = Node('text', 'o hai', 0, 5)
#     good = '<Node called "text" matching "o hai">'
#     eq_(str(n), good)
#     eq_(unicode(n), good)
 
# TODO: The difference between str and unicode in Julia? None?

n = Node("text", "o hai", 1, 5)
good = "<Node called 'text' matching 'o hai'>"

println("About to find out if string(n) is equal to good")
@show good
println(n)
@show n
@show string(n)
@test string(n) == good


# 
# def test_repr():
#     """Test repr of ``Node``."""
#     s = u'hai ö'
#     boogie = u'böogie'
#     n = Node(boogie, s, 0, 3, children=[
#             Node('', s, 3, 4), Node('', s, 4, 5)])
#     eq_(repr(n), """s = {hai_o}\nNode({boogie}, s, 0, 3, children=[Node('', s, 3, 4), Node('', s, 4, 5)])""".format(hai_o=repr(s), boogie=repr(boogie)))

s = "hai ö"
boogie = "böogie"
n = Node(boogie, s, 1, 3, [
        Node("", s, 4, 3), Node("", s, 5, 4)])
shouldbe = "\"s = \\\"hai ö\\\" ; Node{:böogie}(1, 3, [Node{:}(4, 3, []), Node{:}(5, 4, [])])\""
@test repr(n) == shouldbe
# More test I wrote.
# Test
mytext = "this is my text"
copytext = string(mytext)








n = Node("myexpr", mytext, 5, 8)
n2 = Node("myexpr", mytext, 5, 8)
n3 = Node("myexpr2", mytext, 5, 8)
nct = Node("myexpr", copytext, 5, 8)

@test length(n) == 0
@test isa(n, Node)
@test nodetext(n) == " is "
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
@test Nodes.indent(Nodes.indent("foo","|"),"|") == "||foo"
@test Nodes.indent("foo\nbar","  ") == "  foo\n  bar"

# TODO: test RegexNode
@show n2
for n in n2
    @show n
end
println(Nodes.prettily(n2, n3))
println(Nodes.prettily(n2, Node()))

type SomeVisitor <: NodeVisitor end
@test_throws visit(SomeVisitor(), n2)
# @test lift_child(SomeVisitor(), n2, "foo") == "foo"

# keyword syntax -- Can't work and also have positional work
# nwc = Node("withchildren", mytext, 1, 4, children=[n n2 n3])
# @test length(nwc.children) == 3
# nwm = Node("withmatch", mytext, 1, 4, match=match(r"\S+", mytext))
# @test nwm.match != nothing
# nwcm = Node("with_match_and_children", mytext, 1, 4, match=match(r"\S+", mytext), children=[n n2 n3])
# @test nwcm.match != nothing
# @test length(nwcm.children) == 3
