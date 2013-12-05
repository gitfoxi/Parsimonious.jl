
# I'm a module so that I can re-include Expressions every time so I get updates to the code
# But it still doesn't seem to work. :(
module test_expressions

reload("Nodes.jl")
reload("Expressions.jl")
reload("Grammars.jl")

using Base.Test
using Base.typeof
import Grammars.Grammar
import Expressions: Literal, Regex, Sequence, OneOf, Not, Optional, ZeroOrMore, OneOrMore, Expression, parse
using Nodes

function len_eq(node, length)
    node_length = node._end - node.start + 1
    if node_length != length
        println("node_length: ", node_length, " length", length)
    end
    return node_length == length
end

# -- Length Tests --

# Literal tests
@test len_eq(match(Literal("hello"), "ehello", 2), 5)
# Not sure if it's possible to have a match-nothing literal
# @test len_eq(match(Literal(""), ""), 0)
@test len_eq(match(Literal("a"), "a"), 1)

# Regex tests
@test len_eq(match(Regex("hello*"), "hellooo"), 7)  # *
@test len_eq(match(Regex("hello*"), "hellooono"), 7)  # *
@test_throws match(Regex("hello*"), "goodbye")  # no match
@test len_eq(match(Regex("hello", options="i"), "HELLO"), 5)

# test_sequence:
@test len_eq(match(Sequence(Regex("hi*"), Literal("lo"), Regex(".ingo")), "hiiiilobingo1234"), 12)  # succeed
@test_throws len_eq(match(Sequence(Regex("hi*"), Literal("lo"), Regex(".ingo")), "ohiiiilobingo1234"), 12)  # fail
@test_throws match(Sequence(Regex("hi*"), Literal("lo"), Regex(".ingo")), "hiiiilobing")  # don"t
@test len_eq(match(Sequence(Regex("hi*")), ">hiiii", 2), 5)  # non-0 pos
#
## test_one_of:
@test len_eq(match(OneOf(Literal("aaa"), Literal("bb")), "aaa"), 3)  # first alternative
@test len_eq(match(OneOf(Literal("aaa"), Literal("bb")), "bbaaa"), 2)  # second
@test_throws match(OneOf(Literal("aaa"), Literal("bb")), "aa")  # no match
#
## test_not:
@test len_eq(match(Not(Regex(".")), ""), 0)  # match
@test_throws match(Not(Regex(".")), "Hi")  # don"t
#
## test_optional:
@test len_eq(match(Sequence(Optional(Literal("a")), Literal("b")), "b"), 1)  # contained expr fails
@test len_eq(match(Sequence(Optional(Literal("a")), Literal("b")), "ab"), 2)  # contained expr succeeds
#
## test_zero_or_more:
@test len_eq(match(ZeroOrMore(Literal("b")), ""), 0)  # zero
@test len_eq(match(ZeroOrMore(Literal("b")), "bbb"), 3)  # more
#
@test len_eq(match(Regex("^"), ""), 0)  # Validate the next test.
#
# Try to make it loop infinitely using a zero-length contained expression:
@test len_eq(match(ZeroOrMore(Regex("^")), ""), 0)
#
## test_one_or_more:
@test len_eq(match(OneOrMore(Literal("b")), "b"), 1)  # one
@test len_eq(match(OneOrMore(Literal("b")), "bbb"), 3)  # more
@test len_eq(match(OneOrMore(Literal("b"), _min=3), "bbb"), 3)  # with custom min; success
@test_throws match(OneOrMore(Literal("b"), _min=3), "bb")  # with custom min; failure
@test len_eq(match(OneOrMore(Regex("^")), "bb"), 0)  # attempt infinite loop

# -- TreeTests --
#    """Tests for building the right trees
#
#    We have only to test successes here; failures (None-returning cases) are
#    covered above.
#
#    """
# test_simple_node
#        """Test that leaf expressions like ``Literal`` make the right nodes."""
h = Literal("hello", name="greeting")
m = match(h, "hello")
n = Node("greeting", "hello", 1, 5)
@show m
@show n
@test isequal(m, n)
#
# test_sequence_nodes
#        """Assert that ``Sequence`` produces nodes with the right children."""
# TODO: Inconsistent ways of passing 'name' to Sequence and Literal constructors. Make them all like Sequence
s = Sequence(Literal("heigh", name="greeting1"), Literal("ho", name="greeting2"), name="dwarf")
text = "heighho"
@show match(s, text)
@show Node("dwarf", text, 1, 7, [Node("greeting1", text, 1, 5), Node("greeting2", text, 6, 7)])
@test isequal(match(s, text), Node("dwarf", text, 1, 7, [Node("greeting1", text, 1, 5), Node("greeting2", text, 6, 7)]))
#
# test_one_of
#        """``OneOf`` should return its own node, wrapping the child that succeeds."""
o = OneOf(Literal("a", name="lit"), name="one_of")
text = "aa"
@test isequal(match(o, text), Node("one_of", text, 1, 1, [Node("lit", text, 1, 1)]))
#
# test_optional
#        """``Optional`` should return its own node wrapping the succeeded child."""
expr = Optional(Literal("a", name="lit"), name="opt")
text = "a"
@test isequal(match(expr, text), Node("opt", text, 1, 1, [Node("lit", text, 1, 1)]))
#
#        # Test failure of the Literal inside the Optional; the
#        # LengthTests.test_optional is ambiguous for that.
text = ""
@test isequal(match(expr, text), Node("opt", text, 1, 0))
#
# test_zero_or_more_zero
#        """Test the 0 case of ``ZeroOrMore``; it should still return a node."""
expr = ZeroOrMore(Literal("a"), name="zero")
text = ""
@test isequal(match(expr, text), Node("zero", text, 1, 0))
#
# test_one_or_more_one
#        """Test the 1 case of ``OneOrMore``; it should return a node with a child."""
expr = OneOrMore(Literal("a", name="lit"), name="one")
text = "a"
isequal(match(expr, text), Node("one", text, 1, 1, [Node("lit", text, 1, 1)]))
#
#    # Things added since Grammar got implemented are covered in integration
#    # tests in test_grammar.

#class ParseTests(TestCase):
#    """Tests for the ``parse()`` method"""
#
#    def test_parse_success(self):
#        """Make sure ``parse()`` returns the tree on success.
#
#        There's not much more than that to test that we haven't already vetted
#        above.
#
#        """
expr = OneOrMore(Literal("a", name="lit"), name="more")
text = "aa"
isequal(parse(expr, text), Node("more", text, 1, 2, [Node("lit", text, 1, 1), Node("lit", text, 2, 2)]))
#
#
#class ErrorReportingTests(TestCase):
#    """Tests for reporting parse errors"""
#
#    def test_inner_rule_succeeding(self):
#        """Make sure ``parse()`` fails and blames the
#        rightward-progressing-most named Expression when an Expression isn"t
#        satisfied.
#
#        Make sure ParseErrors have nice Unicode representations.
#
#        """

# TODO: use this when Grammar is working
#grammar = Grammar("
#            bold_text = open_parens text close_parens
#            open_parens = '(('
#            text = ~'[a-zA-Z]+'
#            close_parens = '))'
#            ")
#text = "((fred!!"
#parse(grammar, text)
#try
#    parse(grammar, text)
#catch e
#    isequal(e.pos, 6)
#    isequal(e.expr, grammar["close_parens"])
#    isequal(e.text, text)
##    isequal(unicode(e), u"Rule "close_parens" didn"t match at "!!" (line 1, column 7).")
#end

#
#    def test_rewinding(self):
#        """Make sure rewinding the stack and trying an alternative (which
#        progresses farther) from a higher-level rule can blame an expression
#        within the alternative on failure.
#
#        There"s no particular reason I suspect this wouldn"t work, but it"s a
#        more real-world example than the no-alternative cases already tested.
#
#        """
#        grammar = Grammar("""
#            formatted_text = bold_text / weird_text
#            bold_text = open_parens text close_parens
#            weird_text = open_parens text "!!" bork
#            bork = "bork"
#            open_parens = "(("
#            text = ~"[a-zA-Z]+"
#            close_parens = "))"
#            """)
#        text = "((fred!!"
#        try:
#            grammar.parse(text)
#        except ParseError as error:
#            eq_(error.pos, 8)
#            eq_(error.expr, grammar["bork"])
#            eq_(error.text, text)
#
#    def test_no_named_rule_succeeding(self):
#        """Make sure ParseErrors have sane printable representations even if we
#        never succeeded in matching any named expressions."""
#        grammar = Grammar("""bork = "bork"""")
#        try:
#            grammar.parse("snork")
#        except ParseError as error:
#            eq_(error.pos, 0)
#            eq_(error.expr, grammar["bork"])
#            eq_(error.text, "snork")
#
#    def test_parse_with_leftovers(self):
#        """Make sure ``parse()`` reports where we started failing to match,
#        even if a partial match was successful."""
#        grammar = Grammar(r"""sequence = "chitty" (" " "bang")+""")
#        try:
#            grammar.parse("chitty bangbang")
#        except IncompleteParseError as error:
#            eq_(unicode(error), u"Rule "sequence" matched in its entirety, but it didn"t consume all the text. The non-matching portion of the text begins with "bang" (line 1, column 12).")
#
#    def test_favoring_named_rules(self):
#        """Named rules should be used in error messages in favor of anonymous
#        ones, even if those are rightward-progressing-more, and even if the
#        failure starts at position 0."""
#        grammar = Grammar(r"""starts_with_a = &"a" ~"[a-z]+"""")
#        try:
#            grammar.parse("burp")
#        except ParseError as error:
#            eq_(unicode(error), u"Rule "starts_with_a" didn"t match at "burp" (line 1, column 1).")
#
#    def test_line_and_column(self):
#        """Make sure we got the line and column computation right."""
#        grammar = Grammar(r"""
#            whee_lah = whee "\n" lah "\n"
#            whee = "whee"
#            lah = "lah"
#            """)
#        try:
#            grammar.parse("whee\nlahGOO")
#        except ParseError as error:
#            # TODO: Right now, this says "Rule <Literal "\n" at 0x4368250432>
#            # didn"t match". That"s not the greatest. Fix that, then fix this.
#            ok_(unicode(error).endswith(ur"""didn"t match at "GOO" (line 2, column 4)."""))
#
#
#class RepresentationTests(TestCase):
#    """Tests for str(), unicode(), and repr() of expressions"""
#
#    def test_unicode_crash(self):
#        """Make sure matched unicode strings don"t crash ``__str__``."""
#        grammar = Grammar(r"string = ~r"\S+"u")
#        str(grammar.parse(u"中文"))
#
#    def test_unicode(self):
#        """Smoke-test the conversion of expressions to bits of rules.
#
#        A slightly more comprehensive test of the actual values is in
#        ``GrammarTests.test_unicode``.
#
#        """
#        unicode(rule_grammar)

end
