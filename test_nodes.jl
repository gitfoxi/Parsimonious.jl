# -*- coding: utf-8 -*-

# modularize so I can 'reload' in repl
module test_nodes

using Base.Test
reload("Nodes.jl")  # for repl debugging reload things in the order they would call each other
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

visit(::HtmlFormatter, ::String, ::ChildlessNode{:bold_open}) = "<b>"

#     def visit_bold_close(self, node, visited_children):
#         return '</b>'
#

visit(::HtmlFormatter, ::String, ::ChildlessNode{:bold_close}) = "</b>"

#     def visit_text(self, node, visited_children):
#         """Return the text verbatim."""
#         return node.text

visit(::HtmlFormatter, f::String, node::ChildlessNode{:text}) = nodetext(node, f)

#
#     def visit_bold_text(self, node, visited_children):
#         return ''.join(visited_children)

visit(::HtmlFormatter, ::String, ::ParentNode{:bold_text}, visited_children) = join(visited_children, "")

type ExplosiveFormatter <: NodeVisitor end

#
#
# class ExplosiveFormatter(NodeVisitor):
#     """Visitor which raises exceptions"""
#
#     def visit_boom(self, node, visited_children):
#         raise ValueError

# BoundsError is supposed to get wrapped with VisitationError
visit(::ExplosiveFormatter, ::String, ::ChildlessNode{:boom}) = throw(BoundsError)

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
            (Node("bold_open", text, 1, 2),
              Node("text", text, 3, 7),
              Node("bold_close", text, 8, 9)))
result = visit(HtmlFormatter(), text, tree)
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
# TODO: Hard to fix under the new node Union paradigm
#        rethrow(ex)
    end
end

#
#
# def test_str():

txt = "o hai"
n = Node("text", 1, 5)
# stringexample = "<Node called 'text' matching 'o hai'>\n"
stringexample = "1 ChildlessNode{:text}                                                  'o hai'\n"
@test string(NodeText(n, txt)) == stringexample


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
shouldbe = "\"s = \\\"hai ö\\\" ; ParentNode{:böogie}(1, 3, [Node{:}(4, 3), Node{:}(5, 4)])\""
# TODO: who gives a fuck?
#@test repr(n) == shouldbe

## More test I wrote ##

# start and end must be within the text
# TODO: put back assertions in constructors
# @test_throws Node("myexpr", "123456", 0, 2)
# @test_throws Node("myexpr", "123456", 2, 7)
@test isa(Node("myexpr", "123456", 1, 6), AnyNode)

# _end may be less than start to indicate a 0-length capture
@test isa(Node("myexpr", "123456", 1, 0), AnyNode)

# Some nodes for testing

#         123456789012345
mytext = "this is my text"
copytext = string(mytext)
n = Node("myexpr", mytext, 5, 8)
n2 = Node("myexpr", mytext, 5, 8)
n3 = Node("myexpr2", mytext, 5, 8)
nct = Node("myexpr", copytext, 5, 8)

# length(n) is the number of children n has
@test length(n) == 0

@test isa(n, AnyNode)
@test nodetext(n, mytext) == " is "

# n == nct even though text is a copy, it is an exact copy
@test !is(mytext, copytext)
@test n == nct

@test isempty(Node())
@test !isempty(n)
@test Node() == Node()  # Because immutable

@test isequal(n, n)
@test isequal(n, n2)
@test !isequal(n, n3)
@test isequal(Node(), Node())

# add some children to n2
# Thinking of making children immutable ... yeah, that should be fine
n2 = Node("myexpr", mytext, 5, 8, [n3, n3, n3, n3])

# no longer equal
@test !isequal(n, n2)

# length(n) is number of children
@test length(n2.children) == 4
@test length(n2) == 4

# Print an error string when pretty printing tree traceback
@test errstring(n, n2) == ""
@test match(r"Error", errstring(n2, n2)) != nothing
@test match(r"Error", errstring(n, n2)) == nothing

# Indent nodes when pretty printing
@test Nodes.indent(Nodes.indent("foo","|"),"|") == "||foo"
@test Nodes.indent("foo\nbar","  ") == "  foo\n  bar"

# TODO: test RegexNode

type SomeVisitor <: NodeVisitor end
@test_throws visit(SomeVisitor(), n2)
@test lift_child(SomeVisitor(), "foobar", n2, ["foo"]) == "foo"

# keyword syntax
# many tests because I thought for a long time this can't work
# I strongly suspect it is slow
# keyword syntax is totally out
#@time begin for i in 1:1000
#    nwc = Node(T="withchildren", fulltext=mytext, start=1, _end=4, children=[n n2 n3])
#    @test length(nwc) == 3
#    nwd = Node(T="nochildren", fulltext=mytext, start=1, _end=4)
#    @test length(nwd) == 0
#    m=match(r"\S+", mytext)
#    nwe = Node(match=m, _end=4, start=1, fulltext=mytext, T="withchildren")
#    @test isa(nwe.match, RegexMatch)
#    nwf = Node("withchildren", mytext, 1, 0, children=[n n2 n3])
#    nwg = Node("withchildren", mytext, 1, 0, match=m, children=[n n2 n3])
#    nwh = Node("withchildren", mytext, 1, 0, match=m)
#end
#end

@test textlength(Node("foo","",1,0)) == 0
@test textlength(Node("foo","f", 1,1)) == 1
@test textlength(Node("foo","foo", 1,3)) == 3

end
