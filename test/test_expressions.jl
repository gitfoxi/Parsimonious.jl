
#reload("Nodes.jl")
#reload("Expressions.jl")
#reload("Parsimonious.jl")

using Base.Test
using Parsimonious

type TestExpression <: Expression
    name
end

# function wrap so I can time running independent of compilation
# function go()

function len_eq(node, len)
    return textlength(node) == len
end

# -- Length Tests --

# Literal tests
@show m = match(Literal("hello"), "ehello", 2)
@show m.match
@show endof(m.match)
@show textlength(match(Literal("hello"), "ehello", 2))
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
@test isequal(match(s, text), Node("dwarf", text, 1, 7, (Node("greeting1", text, 1, 5), Node("greeting2", text, 6, 7))))
#
# test_one_of
#        """``OneOf`` should return its own node, wrapping the child that succeeds."""
o = OneOf(Literal("a", name="lit"), name="one_of")
text = "aa"
@test isequal(match(o, text), Node("one_of", text, 1, 1, (Node("lit", text, 1, 1),)))
#
# test_optional
#        """``Optional`` should return its own node wrapping the succeeded child."""
expr = Optional(Literal("a", name="lit"), name="opt")
text = "a"
@test isequal(match(expr, text), Node("opt", text, 1, 1, (Node("lit", text, 1, 1),)))
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
@test match(expr, text) == Node("zero", text, 1, 0)
#match(expr,text) => ParentNode{:zero}(ZeroLengthMatch(1),())
#Node("zero",text,1,0) => ChildlessNode{:zero}(ZeroLengthMatch(1))
#Not sure if it should be Parent or Childless
#@test isequal(match(expr, text), Node("zero", text, 1, 0))

# test_one_or_more_one
#        """Test the 1 case of ``OneOrMore``; it should return a node with a child."""
expr = OneOrMore(Literal("a", name="lit"), name="one")
text = "a"
isequal(match(expr, text), Node("one", text, 1, 1, (Node("lit", text, 1, 1),)))
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
isequal(parse(expr, text), Node("more", text, 1, 2, (Node("lit", text, 1, 1), Node("lit", text, 2, 2),)))
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

# Try examples that will work on both Boot Grammar and Rule Grammar on both
boot_gr, rule_gr = Parsimonious.boot_grammar(), Grammar()
for (rules, file) in zip((boot_gr, rule_gr), ("boot_expr", "rule_expr"))
    print(IOString(), rules.default_rule)  # print no infinite loop
end

# parse / match don't crash
parse(lookup(rule_gr, "rules"), Parsimonious.rule_syntax)
match(lookup(rule_gr, "rules"), Parsimonious.rule_syntax)

#for g in [boot_gr, rule_gr]
for g in [rule_gr]
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
end  # for g in ...

#    def test_parse_with_leftovers(self):
#        """Make sure ``parse()`` reports where we started failing to match,
#        even if a partial match was successful."""

# TODO: crap, this one won't compile -- first time using parenthesis
# ERROR: Rule 'equals' didn't match at ' = 'chitty' (' ' 'ban' (line 1, column 9).
grammar = Grammar(rule_gr, """sequence = 'chitty' (' ' 'bang')+""")
try
    parse(grammar, "chitty bangbang")
catch error
    @test unicode(error) == "Rule 'sequence' matched in its entirety, but it didn't consume all the text. The non-matching portion of the text begins with 'bang' (line 1, column 11)."
end


#    def test_favoring_named_rules(self):
#        """Named rules should be used in error messages in favor of anonymous
#        ones, even if those are rightward-progressing-more, and even if the
#        failure starts at position 0."""

# TODO: Another fail. Something very wrong.
grammar = Grammar(rule_gr, """starts_with_a = &'a' ~'[a-z]+'""")
try
    parse(grammar, "burp")
catch error
    @test unicode(error) == "Rule 'starts_with_a' didn't match at 'burp' (line 1, column 1)."
end

#    def test_line_and_column(self):
#        """Make sure we got the line and column computation right."""

# TODO: This is fucked. I'm not escaping literals which is good for
# parsing the rule syntax but it makes a problem for texts like this.
# Check how python interprets these strings.
# Python escapes \ so you have \\n
# Julia does this and also escapes quotes so you have \"
# but when you print it \" prints correctly. This may be a subtle bug affecting
# Regex and other Julia string processing.
# TODO: Also the unicode error messages quadrouple escape everythig
# TODO: Document the exact difference between p", r", p""", r""", " and """

grammar = Grammar(rule_gr, p"""
    whee_lah = whee "\n" lah "\n"
    whee = "whee"
    lah = "lah"
    """)
print(grammar.default_rule)
# TODO: This text does parse which is good. Need to revisit escaping issues
# To parse JSON you definitely need to escape quotes.
parse(grammar, p"whee\nlah\n")

try
    parse(grammar, p"whee\nlahGOO")
catch error
    # TODO: Right now, this says "Rule <Literal "\n" at 0x4368250432>
    # didn"t match". That"s not the greatest. Fix that, then fix this.
    #ok_(unicode(error).endswith(ur"""didn"t match at "GOO" (line 2, column 4)."""))
    @show unicode(error)
# TODO: Thinks line 1, column 1 -- but also, parsing the proper text fails
#    @test endswith(unicode(error), """didn"t match at "GOO" (line 2, column 4).""")
end


#class RepresentationTests(TestCase):
#    """Tests for str(), unicode(), and repr() of expressions"""
#
#    def test_unicode_crash(self):
#        """Make sure matched unicode strings don"t crash ``__str__``."""
grammar = Grammar(rule_gr, p"""string = ~"\S+" """)
@show grammar.default_rule
println(parse(grammar, "中文"))

grammar = Grammar(rule_gr, p"""string = ~"\S\S" """)
@show grammar.default_rule
println(parse(grammar, "中文"))

grammar = Grammar(rule_gr, s"""string = ~"xx" """)
@show grammar.default_rule
println(parse(grammar, "xx"))

grammar = Grammar(rule_gr, s"""string = ~"中文" """)
@show grammar.default_rule
println(parse(grammar, "中文"))

grammar = Grammar(rule_gr, p"""string = "中文" """)
@show grammar.default_rule
# TODO: Failing parse grammar.
println(parse(grammar, "中文"))

# TODO: mix character width like as中文df

#
#    def test_unicode(self):
#        """Smoke-test the conversion of expressions to bits of rules.
#
#        A slightly more comprehensive test of the actual values is in
#        ``GrammarTests.test_unicode``.
#
#        """
#        unicode(rule_grammar)

mye = ParseError("foo\nfoo\n", TestExpression("tst"), 5)
@test Parsimonious.line(mye) == 2
@test Parsimonious.column(mye) == 1
mye = ParseError("foo\nfoo\n", TestExpression("tst"), 4)
@test Parsimonious.line(mye) == 1
@test Parsimonious.column(mye) == 4
myf = ParseError("foo\nfoo\n", TestExpression("tst"), 1)
@test Parsimonious.line(myf) == 1
@test Parsimonious.column(myf) == 1
myio = IOString()
showerror(myio, myf)
seekstart(myio)
# TODO: Fucked up because of not escaping strings:
# "Rule 'tst' didn't match at 'foo\n"
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



@test parse(Regex("fo.*", options="", name="myregex"), "fooooo", 1) == Node("myregex", "fooooo", 1, 6)  #, match=match(r"fo.*", "fooooo"))
@test_throws parse(Regex("fo.*", options="", name="myregex"), "FOOOOO", 1)
@test parse(Regex("fo.*", options="i", name="myregex"), "FOOOOO", 1) == Node("myregex", "FOOOOO", 1, 6)  #, match=match(r"fo.*"i, "FOOOOO"))
@test parse(Sequence("seq", l, l), "foofoo", 1) == Node("seq", "foofoo", 1, 6, (Node("foo", "foofoo", 1, 3), Node("foo", "foofoo", 4, 6)))
@test parse(OneOf(Literal("foo"), Literal("bar")), "bar", 1) == Node("", "bar", 1, 3, (Node("", "bar", 1, 3),))
@test parse(Sequence(Lookahead(Literal("fo")), Literal("foo")), "foo") == Node("", "foo", 1, 3, (Node("", "foo", 1, 0), Node("", "foo", 1, 3)))
@test parse(Sequence(Not(Literal("bar")), Literal("foo")), "foo") == Node("", "foo", 1, 3, (Node("", "foo", 1, 0), Node("", "foo", 1, 3)))
@test parse(Sequence(Optional(Literal("bar")), Literal("foo")), "foo") == Node("", "foo", 1, 3, (Node("", "foo", 1, 0), Node("", "foo", 1, 3)))
@test parse(Sequence(Optional(Literal("bar")), Literal("foo")), "barfoo") == Node("", "barfoo", 1, 6, (Node("", "barfoo", 1, 3, (Node("", "barfoo", 1, 3),)), Node("", "barfoo", 4, 6)))
# TODO: mixed UTF8 for all these tests
@show parse(ZeroOrMore(Literal("bar")), "")
@test parse(ZeroOrMore(Literal("bar")), "") == Node("", "", 1, 0) # Again, not sure if it should have children
@test parse(ZeroOrMore(Literal("bar")), "bar") == Node("", "bar", 1, 3, (Node("", "bar", 1, 3),))
@test parse(ZeroOrMore(Literal("bar")), "barbar") == Node("", "barbar", 1, 6, (Node("", "barbar", 1, 3), Node("", "barbar", 4, 6)))
@test_throws parse(OneOrMore(Literal("bar")), "")
@test parse(OneOrMore(Literal("bar")), "barbar") == Node("", "barbar", 1, 6, (Node("", "barbar", 1, 3), Node("", "barbar", 4, 6)))
@test parse(OneOrMore(Literal("bar")), "bar") == Node("", "bar", 1, 3, (Node("", "bar", 1, 3),))
@test_throws parse(OneOrMore(Literal("bar")), "bas")
te = TestExpression("tst")
@test Sequence("", te, te) == Sequence(te, te, name="") == Sequence(te, te)

# end  # function go()
# go()

