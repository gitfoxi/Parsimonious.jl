
module Grammar

export MyGrammar

import Expressions: Literal, Regex, Sequence, OneOf, Lookahead, Optional, ZeroOrMore, OneOrMore, Not, Expression, parse
# NodeVisitor is abstract and it tells me I can't import it
import Nodes: Node, NodeVisitor

type MyGrammar
    rules::ASCIIString
    default_rule::ASCIIString
    exprs::Dict
end

function MyGrammar(rules::ASCIIString, default_rule=nothing)
    exprs, first = _expressions_from_rules(rules)
    if isa(default_rule, Nothing)
        default_rule = first.name
    end
    MyGrammar(rules, default_rule, exprs)
end

function _expressions_from_rules(rules::ASCIIString)
    # TODO

    first = Literal("foo")
    rules = {"foo" => first}

    return rules, first
end

typealias BootstrappingGrammar Grammar

function _bootstrapping_expressions_from_rules()
    # TODO: I shouldn't have to provide the leading ^
    # TODO: Swap single and double quote syntax since Julia insists on
    # double quotes
    rule_syntax = ""
    more_rule_syntax = "
        # Ignored things (represented by _) are typically hung off the end of the
        # leafmost kinds of nodes. Literals like '/' count as leaves.

        rules = _ rule+
        rule = label equals expression
        equals = '=' _
        literal = spaceless_literal _

        # So you can't spell a regex like `~'...' ilm`:
        spaceless_literal = ~'u?r?\'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\''is /
                            ~'u?r?'[^'\\\\]*(?:\\\\.[^'\\\\]*)*''is

        expression = ored / sequence / term
        or_term = '/' _ term
        ored = term or_term+
        sequence = term term+
        not_term = '!' term _
        lookahead_term = '&' term _
        term = not_term / lookahead_term / quantified / atom
        quantified = atom quantifier
        atom = reference / literal / regex / parenthesized
        regex = '~' spaceless_literal ~'[ilmsux]*'i _
        parenthesized = '(' _ expression ')' _
        quantifier = ~'[*+?]' _
        reference = label !equals

        # A subsequent equal sign is the only thing that distinguishes a label
        # (which begins a new rule) from a reference (which is just a pointer to a
        # rule defined somewhere else):
        label = ~'[a-zA-Z_][a-zA-Z_0-9]*' _

        # _ = ~r'\s*(?:#[^\r\n]*)?\s*'
        _ = meaninglessness*
        meaninglessness = ~r'\s+' / comment
        comment = ~r'#[^\r\n]*'
        "
    comment = Regex("^#[^\r\n]*", name="comment")
    meaninglessness = OneOf(Regex("^\s+"), comment, name="meaninglessness")
    _ = ZeroOrMore(meaninglessness, name="_")
    equals = Sequence(Literal("="), _, name="equals")
    label = Sequence(Regex("^[a-zA-Z_][a-zA-Z_0-9]*"), _, name="label")
    reference = Sequence(label, Not(equals), name="reference")
    quantifier = Sequence(Regex("^[*+?]"), _, name="quantifier")
    spaceless_literal = Regex("^u?r?'[^'\]*(?:\\.[^'\]*)*'",
        options="is",
        name="spaceless_literal")
    literal = Sequence(spaceless_literal, _, name="literal")
    regex = Sequence(Literal("~"),
                     literal,
                     Regex("[ilmsux]*", options="i"),
                     _,
                     name="regex")
    atom = OneOf(reference, literal, regex, name="atom")
    quantified = Sequence(atom, quantifier, name="quantified")

    term = OneOf(quantified, atom, name="term")
    not_term = Sequence(Literal("!"), term, _, name="not_term")

    sequence = Sequence(term, OneOrMore(term), name="sequence")
    or_term = Sequence(Literal("/"), _, term, name="or_term")
    ored = Sequence(term, OneOrMore(or_term), name="ored")
    expression = OneOf(ored, sequence, term, name="expression")
    rule = Sequence(label, equals, expression, name="rule")
    rules = Sequence(_, OneOrMore(rule), name="rules")

    rule_tree = parse(rules, rule_syntax)
end


# TODO: something like """ for quoting?
g = MyGrammar("
            polite_greeting = greeting ', my good ' title
            greeting        = 'Hi' / 'Hello'
            title           = 'madam' / 'sir'
            ")
_bootstrapping_expressions_from_rules()
end
