
module Grammars

export Grammar

import Expressions: Literal, Regex, Sequence, OneOf, Lookahead, Optional, ZeroOrMore, OneOrMore, Not, Expression, parse
# NodeVisitor is abstract and it tells me I can't import it
import Nodes: Node, NodeVisitor, pprettily

type Grammar
    rules::ASCIIString
    default_rule::ASCIIString
    exprs::Dict
end

function Grammar(rules::ASCIIString, default_rule=nothing)
    exprs, first = _expressions_from_rules(rules)
    if isa(default_rule, Nothing)
        default_rule = first.name
    end
    Grammar(rules, default_rule, exprs)
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
    # I doubled up the \\ and in some cases (if not all) that's the wrong thing to do
    # I don't want to use r"regex" syntax because that forces regex compiling and then I would have
    # to recomile to prepend the ^. But that may be an okay short-term solution. Also, I can
    # invent a different type of quote (or see if someone has) which preserves \ exactly so that
    # '\s' doesn't turn into 's' which was causing me headaches
    rule_syntax = """
        # Ignored things (represented by _) are typically hung off the end of the
        # leafmost kinds of nodes. Literals like '/' count as leaves.

        rules = _ rule+
        rule = label equals expression
        equals = '=' _
        literal = spaceless_literal _

        # So you can't spell a regex like `~'...' ilm`:
        spaceless_literal = ~'u?r?\\'[^\\'\\\\\\\\]*(?:\\\\\\\\.[^\\'\\\\\\\\]*)*\\''is / 
                            ~'u?r?\"[^"\\\\\\\\]*(?:\\\\\\\\.[^"\\\\\\\\]*)*\"'is

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

        # _ = ~r'\\s*(?:#[^\\r\\n]*)?\\s*'
        _ = meaninglessness*
        meaninglessness = ~r'\\s+' / comment
        comment = ~r'#[^\\r\\n]*'
        """
    comment = Regex("#[^\\r\\n]*", name="comment")
    meaninglessness = OneOf(Regex("\\s+"), comment, name="meaninglessness")
    _ = ZeroOrMore(meaninglessness, name="_")
    equals = Sequence(Literal("="), _, name="equals")
    label = Sequence(Regex("[a-zA-Z_][a-zA-Z_0-9]*"), _, name="label")
    reference = Sequence(label, Not(equals), name="reference")
    quantifier = Sequence(Regex("[*+?]"), _, name="quantifier")
    spaceless_literal = Regex("u?r?'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'",
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
    unshift!(term.members, not_term)

    sequence = Sequence(term, OneOrMore(term), name="sequence")
    or_term = Sequence(Literal("/"), _, term, name="or_term")
    ored = Sequence(term, OneOrMore(or_term), name="ored")
    expression = OneOf(ored, sequence, term, name="expression")
    rule = Sequence(label, equals, expression, name="rule")
    rules = Sequence(_, OneOrMore(rule), name="rules")

    # Turn the parse tree into a map of expressions:
    rule_tree = parse(rules, rule_syntax)
    println("LETS GO VISIT THE RULE TREE")
    visit(RuleVisitor(), rule_tree)

    # try_tree = parse(""")
end

type RuleVisitor <: NodeVisitor end

# Same as visit(v::NodeVisitor, node::Node) but here for debugging ...
function visit(v::NodeVisitor, node::Node)
    @show "rulevisitor", typeof(node)
    visited_children = [visit(v, child) for child in node]
    visit(v, node, visited_children)
end

# generic_visit - no children, just return node
# except this overloads function visit(v::NodeVisitor, node::Node) and ruins everything
#    @show "childless visit"
#    pprettily(node)
#    return node
#end

# remove after debug
function visit(v::RuleVisitor, ::Node{:meaninglessness})
    println("MEANINGLESSNESS")
end

# generic_visit - some children, just replace node with its children
function visit(v::RuleVisitor, node::Node, visited_children...)
    @show "generic parent visit"
    pprettily(node)
    return visited_children
end

function visit_quantifier(v::RuleVisitor, n::Node{:quantifier}, symbol, _)
    @show "visit_quantifier", symbol
    pprettily(node)
    symbol
end

function visit_quantified(v::RuleVisitor, n::Node{:quantified}, atom, quantifier)
    @show "visit_quantified"
    pprettily(node)
    if quantifier == '?'
        return Optional(atom)
    elseif quantifier == '*'
        return ZeroOrMore(atom)
    elseif quantifier == '+'
        return OneOrMore(atom)
    end
    error("How is '" * quantifier * "' a quantifier to you?")
end

g = Grammar("""
            polite_greeting = greeting ', my good ' title
            greeting        = 'Hi' / 'Hello'
            title           = 'madam' / 'sir'
            """)
_bootstrapping_expressions_from_rules()
end
