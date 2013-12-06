
# I'm a module so that I can re-include Expressions every time so I get updates to the code
# But it still doesn't seem to work. :(
module test_expressions

reload("Nodes.jl")
reload("Expressions.jl")
reload("Grammars.jl")

using Base.Test
using Base.typeof
# TODO: using Grammars, no import
import Grammars.Grammar
import Grammars
# import Expressions: Literal, Regex, Sequence, OneOf, Not, Optional, ZeroOrMore, OneOrMore, Expression, parse
using Nodes
using Expressions
using Grammars

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
@test len_eq(match(Literal("aaa"), "aaa"), 3)  # first alternative
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
@test isequal(m, n)
#
# test_sequence_nodes
#        """Assert that ``Sequence`` produces nodes with the right children."""
# TODO: Inconsistent ways of passing 'name' to Sequence and Literal constructors. Make them all like Sequence
s = Sequence(Literal("heigh", name="greeting1"), Literal("ho", name="greeting2"), name="dwarf")
text = "heighho"
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

for g in [Grammars.boot_grammar(), Grammar()]  # rule_grammar -- when working
g = Grammars.boot_grammar()
    grammar = Grammar(g, "
                bold_text = open_parens text close_parens
                open_parens = '(('
                text = ~'[a-zA-Z]+'
                close_parens = '))'
                ")

    text = "((fred))"
    parse(grammar, text)
    text = "((fred!!"
    try
        parse(grammar, text)
    catch e
        @test e.pos == 7
        @test e.expr == lookup(grammar, "close_parens")
        @test e.text == text
        @test unicode(e) == "Rule 'close_parens' didn't match at '!!' (line 1, column 7)."
    end

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
    grammar = Grammar(g, """
        formatted_text = bold_text / weird_text
        bold_text = open_parens text close_parens
        weird_text = open_parens text '!!' bork
        bork = 'bork'
        open_parens = '(('
        text = ~'[a-zA-Z]+'
        close_parens = '))'
        """)

    text = "((fred))"
    parse(grammar, text)
    text = "((fred!!"
    try
        parse(grammar, text)
    catch error
        @test error.pos == 9
        @test error.expr == lookup(grammar, "bork")
        @test error.text == text
    end
#
#    def test_no_named_rule_succeeding(self):
#        """Make sure ParseErrors have sane printable representations even if we
#        never succeeded in matching any named expressions."""
    grammar = Grammar(g, "bork = 'bork'")
    try
        parse(grammar, "snork")
    catch error
        @test error.pos == 1
        @test error.expr == lookup(grammar, "bork")
        @test error.text == "snork"
    end
end

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

type TestExpression <: Expression
    name
end
mye = ParseError("foo\nfoo\n", TestExpression("tst"), 5)
@test Expressions.line(mye) == 2
@test Expressions.column(mye) == 1
mye = ParseError("foo\nfoo\n", TestExpression("tst"), 4)
@test Expressions.line(mye) == 1
@test Expressions.column(mye) == 4
myf = ParseError("foo\nfoo\n", TestExpression("tst"), 1)
@test Expressions.line(myf) == 1
@test Expressions.column(myf) == 1
myio = IOString()
showerror(myio, myf)
seekstart(myio)
@test readline(myio) == "Rule 'tst' didn't match at 'foo\\nfoo\\n' (line 1, column 1)."

# Test

l = Literal("foo", name="foo")
t = parse(l, "foo", 1)
# see error messages:
@test_throws parse(l, "fooo", 1)
@test_throws parse(l, "bar", 1)
try
    parse(l, "foos", 1)
catch e
    @test typeof(e) == IncompleteParseError
end
try
    parse(l, "bar", 1)
catch e
    @test typeof(e) == ParseError
end

@test parse(Literal("foo"), "foo") == Node("", "foo", 1, 3)
@test parse(Literal("foo", "foorule"), "foo") == Node("foorule", "foo", 1, 3)



@test parse(Regex("fo.*", options="", name="myregex"), "fooooo", 1) == Node("myregex", "fooooo", 1, 6)
@test_throws parse(Regex("fo.*", options="", name="myregex"), "FOOOOO", 1) == Node("myregex", "FOOOOO", 1, 6)
@test parse(Regex("fo.*", options="i", name="myregex"), "FOOOOO", 1) == Node("myregex", "FOOOOO", 1, 6)
@test parse(Sequence("seq", l, l), "foofoo", 1) == Node("seq", "foofoo", 1, 6, [Node("foo", "foofoo", 1, 3), Node("foo", "foofoo", 4, 6), ])
@test parse(OneOf(Literal("foo"), Literal("bar")), "bar", 1) == Node("", "bar", 1, 3, [Node("", "bar", 1, 3)])
@test parse(Sequence(Lookahead(Literal("fo")), Literal("foo")), "foo") == Node("", "foo", 1, 3, [Node("", "foo", 1, 0), Node("", "foo", 1, 3)])
@test parse(Sequence(Not(Literal("bar")), Literal("foo")), "foo") == Node("", "foo", 1, 3, [Node("", "foo", 1, 0) Node("", "foo", 1, 3)])
@test parse(Sequence(Optional(Literal("bar")), Literal("foo")), "foo") == Node("", "foo", 1, 3, [Node("", "foo", 1, 0), Node("", "foo", 1, 3)])
@test parse(Sequence(Optional(Literal("bar")), Literal("foo")), "barfoo") == Node("", "barfoo", 1, 6, [Node("", "barfoo", 1, 3, [Node("", "barfoo", 1, 3)]), Node("", "barfoo", 4, 6)])
@test parse(ZeroOrMore(Literal("bar")), "") == Node("", "", 1, 0)
@test parse(ZeroOrMore(Literal("bar")), "bar") == Node("", "bar", 1, 3, [Node("", "bar", 1, 3)])
@test parse(ZeroOrMore(Literal("bar")), "barbar") == Node("", "barbar", 1, 6, [Node("", "barbar", 1, 3), Node("", "barbar", 4, 6)])
@test_throws parse(OneOrMore(Literal("bar")), "")
@test parse(OneOrMore(Literal("bar")), "barbar") == Node("", "barbar", 1, 6, [Node("", "barbar", 1, 3), Node("", "barbar", 4, 6)])
@test parse(OneOrMore(Literal("bar")), "bar") == Node("", "bar", 1, 3, [Node("", "bar", 1, 3)])
@test_throws parse(OneOrMore(Literal("bar")), "bas")
te = TestExpression("tst")
@test Sequence("", te, te) == Sequence(te, te, name="") == Sequence(te, te)

end
