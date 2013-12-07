

using Base.Test

using Grammars

# BootstrappingGrammarTests
#Tests for the expressions in the grammar that parses the grammar
#definition syntax

# test_quantifier(self):
text = "*"
@show parse(rule_grammar.exprs["quantifier"], text)
"""
@show Node("quantifier", text, 1, 2, children=[Node("", text, 1, 2), Node("_", text, 2, 2)])
@show (rule_grammar["quantifier"].parse(text) == 
    Node("quantifier", text, 1, 2, children=[
        Node("", text, 1, 2), Node("_", text, 2, 2)]))
text = "?"
@show (rule_grammar["quantifier"].parse(text) ==
    Node("quantifier", text, 1, 2, children=[
        Node("", text, 1, 2), Node("_", text, 2, 2)]))
text = "+"
@show(rule_grammar["quantifier"].parse(text) ==
    Node("quantifier", text, 1, 2, children=[
        Node("", text, 1, 2), Node("_", text, 2, 2)]))


# test spaceless_literal(self):
# NOTE: removed a line here
eq_(rule_grammar['spaceless_literal'].parse(text),
    Node('spaceless_literal', text, 0, len(text), children=[
        Node('', text, 0, len(text))]))
text = r'''r"\""'''
eq_(rule_grammar['spaceless_literal'].parse(text),
    Node('spaceless_literal', text, 0, 5, children=[
        Node('', text, 0, 5)]))

# test regex(self):
text = '~"[a-zA-Z_][a-zA-Z_0-9]*"LI'
eq_(rule_grammar['regex'].parse(text),
    Node('regex', text, 0, len(text), children=[
         Node('', text, 0, 1),
         Node('spaceless_literal', text, 1, 25, children=[
             Node('', text, 1, 25)]),
         Node('', text, 25, 27),
         Node('_', text, 27, 27)]))

# test successes(self):
Make sure the PEG recognition grammar succeeds on various inputs.
ok_(rule_grammar['label'].parse('_'))
ok_(rule_grammar['label'].parse('jeff'))
ok_(rule_grammar['label'].parse('_THIS_THING'))

ok_(rule_grammar['atom'].parse('some_label'))
ok_(rule_grammar['atom'].parse('"some literal"'))
ok_(rule_grammar['atom'].parse('~"some regex"i'))

ok_(rule_grammar['quantified'].parse('~"some regex"i*'))
ok_(rule_grammar['quantified'].parse('thing+'))
ok_(rule_grammar['quantified'].parse('"hi"?'))

ok_(rule_grammar['term'].parse('this'))
ok_(rule_grammar['term'].parse('that+'))

ok_(rule_grammar['sequence'].parse('this that? other'))

ok_(rule_grammar['ored'].parse('this / that+ / "other"'))

# + is higher precedence than &, so 'anded' should match the whole
# thing:
ok_(rule_grammar['lookahead_term'].parse('&this+'))

ok_(rule_grammar['expression'].parse('this'))
ok_(rule_grammar['expression'].parse('this? that other*'))
ok_(rule_grammar['expression'].parse('&this / that+ / "other"'))
ok_(rule_grammar['expression'].parse('this / that? / "other"+'))
ok_(rule_grammar['expression'].parse('this? that other*'))

ok_(rule_grammar['rule'].parse('this = that\r'))
ok_(rule_grammar['rule'].parse('this = the? that other* \t\r'))
ok_(rule_grammar['rule'].parse('the=~"hi*"\n'))

ok_(rule_grammar.parse('''
    this = the? that other*
    that = "thing"
    the=~"hi*"
    other = "ahoy hoy"
    '''))


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


tree = rule_grammar.parse('''number = ~"[0-9]+"\n''')
rules, default_rule = RuleVisitor().visit(tree)

text = '98'
eq_(default_rule.parse(text), Node('number', text, 0, 2))

# test undefined_rule(self):
Make sure we throw the right exception on undefined rules.
tree = rule_grammar.parse('boy = howdy\n')
assert_raises(UndefinedLabel, RuleVisitor().visit, tree)

# test optional(self):
tree = rule_grammar.parse('boy = "howdy"?\n')
rules, default_rule = RuleVisitor().visit(tree)

howdy = 'howdy'

# It should turn into a Node from the Optional and another from the
# Literal within.
eq_(default_rule.parse(howdy), Node('boy', howdy, 0, 5, children=[
                                   Node('', howdy, 0, 5)]))


class GrammarTests(TestCase):
Integration-test ``Grammar``: feed it a PEG and see if it works.

# test expressions_from_rules(self):
Test the ``Grammar`` base class's ability to compile an expression
tree from rules.

That the correct ``Expression`` tree is built is already tested in
``RuleGrammarTests``. This tests only that the ``Grammar`` base class's
``_expressions_from_rules`` works.


greeting_grammar = Grammar('greeting = "hi" / "howdy"')
tree = greeting_grammar.parse('hi')
eq_(tree, Node('greeting', 'hi', 0, 2, children=[
               Node('', 'hi', 0, 2)]))

# test unicode(self):
Assert that a ``Grammar`` can convert into a string-formatted series
of rules.
grammar = Grammar(r
                  bold_text  = bold_open text bold_close
                  text       = ~"[A-Z 0-9]*"i
                  bold_open  = "(("
                  bold_close = "))"
                  )
lines = unicode(grammar).splitlines()
eq_(lines[0], 'bold_text = bold_open text bold_close')
ok_('text = ~"[A-Z 0-9]*"i%s' % ('u' if version_info >= (3,) else '')
    in lines)
ok_('bold_open = "(("' in lines)
ok_('bold_close = "))"' in lines)
eq_(len(lines), 4)

# test match(self):
Make sure partial-matching (with pos) works.
grammar = Grammar(r
                  bold_text  = bold_open text bold_close
                  text       = ~"[A-Z 0-9]*"i
                  bold_open  = "(("
                  bold_close = "))"
                  )
s = ' ((boo))yah'
eq_(grammar.match(s, pos=1), Node('bold_text', s, 1, 8, children=[
                                 Node('bold_open', s, 1, 3),
                                 Node('text', s, 3, 6),
                                 Node('bold_close', s, 6, 8)]))

# test bad_grammar(self):
Constructing a Grammar with bad rules should raise ParseError.
assert_raises(ParseError, Grammar, 'just a bunch of junk')

# test comments(self):
Test tolerance of comments and blank lines in and around rules.
grammar = Grammar(r# This is a grammar.

                  # It sure is.
                  bold_text  = stars text stars  # nice
                  text       = ~"[A-Z 0-9]*"i #dude


                  stars      = "**"
                  # Pretty good
                  #Oh yeah.#)  # Make sure a comment doesn't need a
                                  # \n or \r to end.
eq_(list(sorted(str(grammar).splitlines())),
    ['''bold_text = stars text stars''',
     # TODO: Unicode flag is on by default in Python 3. I wonder if we
     # should turn it on all the time in Parsimonious.
     '''stars = "**"''',
     '''text = ~"[A-Z 0-9]*"i%s''' % ('u' if version_info >= (3,)
                                      else '')])

# test multi_line(self):
Make sure we tolerate all sorts of crazy line breaks and comments in
the middle of rules.
grammar = Grammar(
    bold_text  = bold_open  # commenty comment
                 text  # more comment
                 bold_close
    text       = ~"[A-Z 0-9]*"i
    bold_open  = "((" bold_close =  "))"
    )
ok_(grammar.parse('((booyah))') is not None)

# test not(self):
Make sure "not" predicates get parsed and work properly.
grammar = Grammar(r'''not_arp = !"arp" ~"[a-z]+"''')
assert_raises(ParseError, grammar.parse, 'arp')
ok_(grammar.parse('argle') is not None)

# test lookahead(self):
grammar = Grammar(r'''starts_with_a = &"a" ~"[a-z]+"''')
assert_raises(ParseError, grammar.parse, 'burp')

s = 'arp'
eq_(grammar.parse('arp'), Node('starts_with_a', s, 0, 3, children=[
                              Node('', s, 0, 0),
                              Node('', s, 0, 3)]))

# test parens(self):
grammar = Grammar(r'''sequence = "chitty" (" " "bang")+''')
# Make sure it's not as if the parens aren't there:
assert_raises(ParseError, grammar.parse, 'chitty bangbang')

s = 'chitty bang bang'
eq_(str(grammar.parse(s)),
    <Node called "sequence" matching "chitty bang bang">
<Node matching "chitty">
<Node matching " bang bang">
<Node matching " bang">
    <Node matching " ">
    <Node matching "bang">
<Node matching " bang">
    <Node matching " ">
    <Node matching "bang">)

# test resolve_refs_order(self):
Smoke-test a circumstance where lazy references don't get resolved.
grammar = Grammar(
    expression = "(" terms ")"
    terms = term+
    term = number
    number = ~r"[0-9]+"
    )
grammar.parse('(34)')

# test infinite_loop(self):
Smoke-test a grammar that was causing infinite loops while building.

This was going awry because the "int" rule was never getting marked as
resolved, so it would just keep trying to resolve it over and over.


Grammar(
    digits = digit+
    int = digits
    digit = ~"[0-9]"
    number = int
    main = number
    )

# test right_recursive(self):
Right-recursive refs should resolve.
grammar = Grammar(
    digits = digit digits?
    digit = ~r"[0-9]"
    )
ok_(grammar.parse('12') is not None)

# test badly_circular(self):
Uselessly circular references should be detected by the grammar
compiler.
raise SkipTest('We have yet to make the grammar compiler detect these.')
grammar = Grammar(
    foo = bar
    bar = foo
    )

# test parens_with_leading_whitespace(self):
Make sure a parenthesized expression is allowed to have leading
whitespace when nested directly inside another.
Grammar(foo = ( ("c") )).parse('c')

# test single_quoted_literals(self):
Grammar(foo = 'a' '"').parse('a"')
"""