
module Grammars

export Grammar

import Expressions: Literal, Regex, Sequence, OneOf, Lookahead, Optional, ZeroOrMore, OneOrMore, Not, Expression, parse
# NodeVisitor is abstract and it tells me I can't import it
import Nodes: Node, NodeVisitor, pprettily, text

type Grammar
    rules::ASCIIString
    default_rule::ASCIIString
    exprs::Dict
end

function Grammar(rules::ASCIIString, default_rule=nothing)
    exprs, first_rule = _expressions_from_rules(rules)
    default_rule = default_rule || first_rule.name
    Grammar(rules, default_rule, exprs)
end

function _expressions_from_rules(grammar::Grammar, rules::ASCIIString)
    tree = parse(grammar, rules)
    return visit(RuleVisitor(), tree)
end

function Grammar() # Bootstrapping
    exprs, default_rule = _expressions_from_rules(rule_syntax)
    Grammar(rule_syntax, default_rule, exprs)
end

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

function _expressions_from_rules(rule_syntax::ASCIIString)
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
    visit(RuleVisitor(), rule_tree)
end

type RuleVisitor <: NodeVisitor end

# Same as visit(v::NodeVisitor, node::Node) but here for debugging ...
function visit(v::NodeVisitor, node::Node)
    @show "GENERIC CALLED FOR", typeof(node)
    visited_children = [visit(v, child) for child in node]
    @show v
    @show node
    @show visited_children
    if isempty(visited_children)
        return node
    end
    @show visit(v, node, visited_children...)
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
    nothing
end

# generic_visit - some children, just replace node with its children
# also has the problem of overloading visit(v::NodeVisitor, node::Node)
function visit(v::RuleVisitor, node::Node, c1, c2...)
    @show "generic parent visit"
    @show node
    @show c1
    @show c2
    pprettily(node)
    return vcat(c1, c2...)
end

function visit(v::RuleVisitor, n::Node{:parenthesized}, left_paren, _1, expression, right_paren, _2)
    @show "visit parenthesized"
    pprettily(node)
    expression
end

function visit(v::RuleVisitor, n::Node{:quantifier}, symbol, _)
    @show "visit quantifier", symbol
    pprettily(node)
    symbol
end

function visit(v::RuleVisitor, n::Node{:quantified}, atom, quantifier)
    @show "visit quantified", quantifier
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

function visit(v::RuleVisitor, n::Node{:lookahead_term}, ampersand, term, _)
    @show "visit lookahead_term"
    pprettily(node)
    Lookahead(term)
end

function visit(v::RuleVisitor, n::Node{:not_term}, exclamation, term, _)
    @show "visit not_term"
    pprettily(node)
    Not(term)
end

function visit(v::RuleVisitor, n::Node{:rule}, label, equals, expression)
    @show "visit rule"
    pprettily(node)
    Node(text(label), expression)
end

function visit(v::RuleVisitor, n::Node{:sequence}, terms...)
    @show "visit sequence"
    pprettily(n)
    Sequence(terms...)
end

function visit(v::RuleVisitor, n::Node{:ored}, terms...)
    @show "visit ored"
    pprettily(n)
    OneOf(terms...)
end

function visit(v::RuleVisitor, n::Node{:or_term}, slash, _, term)
    @show "visit or_term"
    pprettily(n)
    term
end

function visit(v::RuleVisitor, n::Node{:label}, name, _)
    @show "visit label"
    pprettily(n)
    text(name)
end

type LazyReference label end

_as_rhs(lr::LazyReference) = "<LazyReference to $(lr.label)>"

function visit(v::RuleVisitor, n::Node{:reference}, label, not_equals)
    @show "visit label"
    pprettily(n)
    LazyReference(label)
end

function visit(v::RuleVisitor, n::Node{:regex}, tilde, literal, flags, _)
    @show "visit literal"
    pprettily(n)
    Regex(literal.literal, text(flags))
end

function _resolve_refs(rule_map, expr, unwalked_names, walking_names)
    """Return an expression with all its lazy references recursively
    resolved.

    Resolve any lazy references in the expression ``expr``, recursing into
    all subexpressions. Populate ``rule_map`` with any other rules (named
    expressions) resolved along the way. Remove from ``unwalked_names`` any
    which were resolved.

    :arg walking_names: The stack of labels we are currently recursing
        through. This prevents infinite recursion for circular refs.

    """
    # If it's a top-level (named) expression and we've already walked it,
    # don't walk it again:
    if !isempty(expr.name) && !in(unwalked_names, expr.name)
        # unwalked_names started out with all the rule names in it, so, if
        # this is a named expr and it isn't in there, it must have been
        # resolved.
        return rule_map[expr.name]
    # If not, resolve it:
    elseif isinstance(expr, LazyReference)
        label = text(expr)
        if !in(walking_names, label)
            # We aren't already working on traversing this label:
            try
                reffed_expr = rule_map[label]
            #except KeyError:
            catch
                raise UndefinedLabel(expr)
            end
            rule_map[label] = _resolve_refs( rule_map, reffed_expr, unwalked_names, walking_names + (label,))

            # If we recurse into a compound expression, the remove()
            # happens in there. But if this label points to a non-compound
            # expression like a literal or a regex or another lazy
            # reference, we need to do this here:
            delete!(unwalked_names, label)
        end
        return rule_map[label]
    else
        members = get(expr, "members", [])
        if members
            expr.members = [_resolve_refs(rule_map, m, unwalked_names, walking_names) for m in members]
        end
        if expr.name != ""
            delete!(unwalked_names, expr.name)
        end
        return expr
    end
end

# return dictionary of expressions and a rule name
function visit(v::RuleVisitor, n::Node{:rules}, _, rules...)
    @show "visit rules", rules...
    rule_map = {expr.name => expr for expr in rules...}
    unwalked_names = Set(collect(keys(rule_map))...)
    while !isempty(unswalked_names)
        for rule_name in unwalked_names
            rule_map[rule_name] = _resolve_refs(rule_map, rule_map[rule_name], unwalked_names, (rule_name,))
            delete!(unwalked_names, rule_name)
        end
    end

    return rule_map, rules[0]
end

# TODO: Need a function for escaping \n, \t etc
# TODO: unicode
visit(v::RuleVisitor, n::Node{:spaceless_literal}, spaceless_literal, visited_children) = Literal(text(spaceless_literal))



# rule_grammar = Grammar()
# @show rule_grammar
#pprettily(rule_grammar)
# println("PAUSED"); readline()
# rule_grammar = Grammar(rule_syntax)
# @show rule_grammar
# println("PAUSED"); readline()
#g = Grammar("""
#            polite_greeting = greeting ', my good ' title
#            greeting        = 'Hi' / 'Hello'
#            title           = 'madam' / 'sir'
#            """)
#@show g

# Test
#
# function visit(v::RuleVisitor, n::Node{:rules}, _, rules)
# returns dictionary of expressions and a rule name
asdf = Literal("asdf", name="asdf")
_ = Regex("\\s*", name="_")
equals = Sequence(Literal("="), _, name="equals")
rule = Sequence(asdf, equals, asdf, name="rule")
rules = Sequence(_, OneOrMore(rule), name="rules")
mytext = " asdf=asdf"
node = parse(rules, mytext)
# rules, default_rule = visit(RuleVisitor(), node)
@show typeof(node)
for visitret in visit(RuleVisitor(), node)
    @show visitret
end
#rules, default_rule = visit(RuleVisitor(), node)
#@show rules, default_rule
#@test is(default_rule, ASCIIString)
#@test is(rules, Dict)


end
