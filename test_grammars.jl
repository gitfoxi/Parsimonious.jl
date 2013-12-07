
module test_grammars

# reload("Nodes.jl")
# reload("Expressions.jl")
# reload("Grammars.jl")
using Base.Test

using Grammars
using Expressions
using Nodes
using Util

# BootstrappingGrammarTests
#Tests for the expressions in the grammar that parses the grammar
#definition syntax

# test_quantifier(self):
text = "*"
@test parse(rule_grammar.exprs["quantifier"], text) == Node("quantifier", text, 1, 1, children=[Node("", text, 1, 1, match=match(r".", "*")), Node("_", text, 2, 1)])

# TODO: Whether nodeA == nodeB regardless of "match" to make tests less verbose
# After all, if they match the same text what's the difference?
text = "?"
@test parse(rule_grammar.exprs["quantifier"], text) == Node("quantifier", text, 1, 1, children=[Node("", text, 1, 1, match=match(r".", "?")), Node("_", text, 2, 1)])

text = "+"
@test parse(rule_grammar.exprs["quantifier"], text) == Node("quantifier", text, 1, 1, children=[Node("", text, 1, 1, match=match(r".", "+")), Node("_", text, 2, 1)])

macro dq_str(s) "\"" * s * "\"" end
macro sq_str(s) "'" * s * "'" end
macro s_mstr(s) strip(lstrip(s))  end

function eq_(a::Node, b::Node)
    # Like isequal but ignore match for easier testing
    typeof(a) == typeof(b) || return false
    length(a) == length(b) || return false
    is(a.fulltext, b.fulltext) || isequal(a.fulltext, b.fulltext) || return false
    a.start == b.start && a._end == b._end || return false
    for i in 1:length(a)
        eq_(a.children[i], b.children[i]) || return false
    end
    true
end

# test spaceless_literal(self):
text = dq"anything but quotes#$*&^"
@test eq_(parse(rule_grammar.exprs["spaceless_literal"], text) , Node("spaceless_literal", text, 1, length(text), children=[Node("", text, 1, length(text))]))

# TODO: benchmark visitors with array, tuple and vararg paramters
# TODO: generic visit throw error when called for a node for which visit(::MyVisitor, ::Node{:mynode}) exists

# text = r'''r"\""'''
# text = p"""'''r"\""'''"""
# TODO: Bug in spaceless_literal on this text. Or maybe not. At least it probably reports the wrong character for unconsumed text
# TODO: Cool colorized, unicodized underlining of errors like clang
# text = s""" "\\"" """
text = unescape_string(s""" "\\"" """)
@show text
@show length(text)
@show length(unescape_string(text))
println(text)
println(unescape_string(text))
@test eq_(parse(rule_grammar.exprs["spaceless_literal"], text), Node("spaceless_literal", text, 1, 4, children=[ Node("", text, 1, 4)]))

# test regex(self):
text = p"""~"[a-zA-Z_][a-zA-Z_0-9]*"LI"""

@test eq_(parse(rule_grammar.exprs["regex"], text),
    Node("regex", text, 1, length(text), children=[
         Node("", text, 1, 1),
         Node("spaceless_literal", text, 2, 25, children=[
             Node("", text, 2, 25)]),
         Node("", text, 26, 27),
         Node("_", text, 28, 27)]))

function ok_(n)
    isa(n, Node)
end

# test successes(self):
# Make sure the PEG recognition grammar succeeds on various inputs.
@test ok_(parse(rule_grammar.exprs["label"], "_"))
@test ok_(parse(rule_grammar.exprs["label"], "jeff"))
@test ok_(parse(rule_grammar.exprs["label"], "_THIS_THING"))


@test ok_(parse(rule_grammar.exprs["atom"], s""" "some_label" """))
@test ok_(parse(rule_grammar.exprs["atom"], s""" "some literal" """))
@test ok_(parse(rule_grammar.exprs["atom"], s""" ~"some regex"i """))

@test ok_(parse(rule_grammar.exprs["quantified"], s""" ~"some regex"i* """))
@test ok_(parse(rule_grammar.exprs["quantified"], "thing+"))
@test ok_(parse(rule_grammar.exprs["quantified"], s""" "hi"? """))

@test ok_(parse(rule_grammar.exprs["term"], "this"))
@test ok_(parse(rule_grammar.exprs["term"], "that+"))

@test ok_(parse(rule_grammar.exprs["sequence"], "this that? other"))

@test ok_(parse(rule_grammar.exprs["ored"], s"""this / that+ / "other" """))

# + is higher precedence than &, so 'anded' should match the whole
# thing:
@test ok_(parse(rule_grammar.exprs["lookahead_term"], "&this+"))

@test ok_(parse(rule_grammar.exprs["expression"], "this"))
@test ok_(parse(rule_grammar.exprs["expression"], "this? that other*"))

@test ok_(parse(rule_grammar.exprs["expression"], s""" &this / that+ / "other" """))
@test ok_(parse(rule_grammar.exprs["expression"], s""" this / that? / "other"+ """))
@test ok_(parse(rule_grammar.exprs["expression"], "this? that other*"))

@test ok_(parse(rule_grammar.exprs["rule"], "this = that\r"))
@test ok_(parse(rule_grammar.exprs["rule"], "this = the? that other* \t\r"))
@test ok_(parse(rule_grammar.exprs["rule"], """the=~"hi*"\n"""))

@test ok_(parse(rule_grammar, """
    this = the? that other*
    that = "thing"
    the=~"hi*"
    other = "ahoy hoy"
    """))

"""
class RuleVisitorTests(TestCase):
Tests for ``RuleVisitor``

As I write these, Grammar is not yet fully implemented. Normally, there'd
be no reason to use ``RuleVisitor`` directly.


# test round_trip(self):
Test a simple round trip.

Parse a simple grammar, turn the parse tree into a map of expressions,
and use that to parse another piece of text.

Not everything was implemented yet, but it was a big milestone and a
proof of concept.
"""

import Grammars.RuleVisitor
import Grammars.visit

# TODO: This causes an error because no visit(v, ::Node{:number}, ...) implemented but the error message is messed up.
tree = parse(rule_grammar, """number = ~"[0-9]+"\n""")
rules, default_rule = visit(RuleVisitor(), tree)

text = "98"
@test eq_(parse(default_rule, text), Node("number", text, 1, 2))

# test undefined_rule(self):
# Make sure we throw the right exception on undefined rules.

tree = parse(rule_grammar, "boy = howdy\n")

# TODO: fails to fail:
# Also fails to remove the LazyReferences
# Also, the references are wrong: Where's "howdy"?
#  ({"boy"=>LazyReference("boy")},LazyReference("boy"))
# @test_throws visit(RuleVisitor(), tree)

@test_throws visit(RuleVisitor(), tree)  # ; debug=true)
try
    visit(RuleVisitor(), tree)
catch e
    @test isa(e, Grammars.UndefinedLabel)
end

# test optional(self):
tree = parse(rule_grammar, """boy = "howdy"?\n""")
rules, default_rule = visit(RuleVisitor(), tree)

howdy = "howdy"

# It should turn into a Node from the Optional and another from the
# Literal within.
@test eq_(parse(default_rule, howdy), Node("boy", howdy, 1, 5, children=[Node("", howdy, 1, 5)]))

"""
class GrammarTests(TestCase):
Integration-test ``Grammar``: feed it a PEG and see if it works.

# test expressions_from_rules(self):
Test the ``Grammar`` base class's ability to compile an expression
tree from rules.

That the correct ``Expression`` tree is built is already tested in
``RuleGrammarTests``. This tests only that the ``Grammar`` base class's
``_expressions_from_rules`` works.

"""

greeting_grammar = Grammar(s"""greeting = "hi" / "howdy" """)
tree = parse(greeting_grammar, "hi")
@test eq_(tree, Node("greeting", "hi", 1, 2, children=[Node("", "hi", 1, 2)]))

# test unicode(self):
# Assert that a ``Grammar`` can convert into a string-formatted series of rules.

grammar = Grammar("""
                  bold_text  = bold_open text bold_close
                  text       = ~"[A-Z 0-9]*"is
                  bold_open  = "(("
                  bold_close = "))"
                  """)
lines = split(unicode(grammar), "\n")
function showln(x...)
    show(x...)
    println()
end
[showln(l) for l in lines]

@test lines[1] == "bold_text = bold_open text bold_close"
@show """text = ~"[A-Z 0-9]*"is"""
@test in("""text = ~"[A-Z 0-9]*"is""", lines)
@test in(s"""bold_open = "((" """, lines)
@test in(s"""bold_close = "))" """, lines)
@test length(lines) == 4

# test match(self):
# Make sure partial-matching (with pos) works.
grammar = Grammar("""
                  bold_text  = bold_open text bold_close
                  text       = ~"[A-Z 0-9]*"i
                  bold_open  = "(("
                  bold_close = "))"
                  """)
s = " ((boo))yah"
@test eq_(match(grammar, s, 2), Node("bold_text", s, 2, 8, children=[
                                 Node("bold_open", s, 2, 3),
                                 Node("text", s, 4, 6),
                                 Node("bold_close", s, 7, 8)]))

# test bad_grammar(self):
# Constructing a Grammar with bad rules should raise ParseError.

@assert_raises ParseError Grammar("just a bunch of junk")

# test comments(self):
# Test tolerance of comments and blank lines in and around rules.
grammar = Grammar("""# This is a grammar.

                  # It sure is.
                  bold_text  = stars text stars  # nice
                  text       = ~"[A-Z 0-9]*"i #dude


                  stars      = "**"
                  # Pretty good
                  #Oh yeah.#""")  # Make sure a comment doesn't need a
                                  # \n or \r to end.

# TODO: Maybe re-implement show(io::IO, g::Grammar) to do what str and unicode do. But not right now.
# eq_(list(sorted(str(grammar).splitlines())),


@test sort(split(unicode(grammar), "\n")) ==
    ["""bold_text = stars text stars""",
     s"""stars = "**" """,
     """text = ~"[A-Z 0-9]*"i"""]


# test multi_line(self):
# Make sure we tolerate all sorts of crazy line breaks and comments in
# the middle of rules.
grammar = Grammar("""
    bold_text  = bold_open  # commenty comment
                 text  # more comment
                 bold_close
    text       = ~"[A-Z 0-9]*"i
    bold_open  = "((" bold_close =  "))"
    """)
ok_(parse(grammar, "((booyah))"))

# test not(self):
# Make sure "not" predicates get parsed and work properly.
grammar = Grammar(s""" not_arp = !"arp" ~"[a-z]+" """)
@assert_raises ParseError parse(grammar, "arp")
@test ok_(parse(grammar, "argle"))

# test lookahead(self):
grammar = Grammar(s"""starts_with_a = &"a" ~"[a-z]+" """)
@assert_raises ParseError parse(grammar, "burp")

s = "arp"
@test eq_(parse(grammar, "arp"), Node("starts_with_a", s, 1, 3, children=[
                              Node("", s, 1, 0),
                              Node("", s, 1, 3)]))

# test parens(self):
grammar = Grammar("""sequence = "chitty" (" " "bang")+""")
# Make sure it's not as if the parens aren't there:
@assert_raises ParseError parse(grammar, "chitty bangbang")



end
