
module Grammars

export Grammar, lookup

# TODO: using Expressions
import Expressions: LazyReference, unicode, Literal, Regex, Sequence, OneOf, Lookahead, Optional, ZeroOrMore, OneOrMore, Not, Expression, parse
using Expressions
# NodeVisitor is abstract and it tells me I can't import it
import Nodes: Node, NodeVisitor, pprettily, nodetext, name, lift_child

type Grammar
    rules::ASCIIString
    default_rule::Expression
    exprs::Dict
end

# TODO: Grammar <: Associative
# for now, a lookup method

lookup(g::Grammar, key) = g.exprs[key]

function Grammar(rule_grammar::Grammar, rule_syntax::ASCIIString; default_rule=nothing)
    exprs, first_rule = _expressions_from_rules(rule_grammar, rule_syntax)
    default_rule = is(default_rule,nothing) ? first_rule : exprs[default_rule]
    Grammar(rule_syntax, default_rule, exprs)
end

function Grammar(rule_syntax::ASCIIString)
    Grammar(rule_grammar, rule_syntax)
end

function parse(grammar::Grammar, text::ASCIIString; pos=1)
    # TODO Expressions.parse to support pos parameter
    parse(grammar.default_rule, text)
end

function _expressions_from_rules(grammar::Grammar, rules::ASCIIString)
    tree = parse(grammar, rules)
    exprs, first_rule = visit(RuleVisitor(), tree)

    @show exprs, first_rule
    @show first_rule
    exprs, first_rule
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
    spaceless_literal = ~'\\'[^\\'\\\\\\\\]*(?:\\\\\\\\.[^\\'\\\\\\\\]*)*\\''is /
                        ~'"[^"\\\\\\\\]*(?:\\\\\\\\.[^"\\\\\\\\]*)*"'is

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
    meaninglessness = ~'\\s+' / comment
    comment = ~'#[^\\r\\n]*'
"""

function boot_grammar()
    # Return a stripped-down rules which may be used to parse
    # rule_syntax to generate the complete rule_grammar
    comment = Regex("#[^\\r\\n]*", name="comment")
    meaninglessness = OneOf(Regex("\\s+"), comment, name="meaninglessness")
    _ = ZeroOrMore(meaninglessness, name="_")
    equals = Sequence(Literal("="), _, name="equals")
    label = Sequence(Regex("[a-zA-Z_][a-zA-Z_0-9]*"), _, name="label")
    reference = Sequence(label, Not(equals), name="reference")
    quantifier = Sequence(Regex("[*+?]"), _, name="quantifier")
    spaceless_literal = Regex("'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'",
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
    term = OneOf(not_term, quantified, atom, name="term")

    sequence = Sequence(term, OneOrMore(term), name="sequence")
    or_term = Sequence(Literal("/"), _, term, name="or_term")
    ored = Sequence(term, OneOrMore(or_term), name="ored")
    expression = OneOf(ored, sequence, term, name="expression")
    rule = Sequence(label, equals, expression, name="rule")
    rules = Sequence(_, OneOrMore(rule), name="rules")

    # TODO: Is there a way to get a list of all of the local variables so
    # I don't have to make this dictionary manually?

    #exprnames = split("""
    #    comment meaninglessness _ equals label reference quantifier
    #    spaceless_literal literal regex atom quantified term not_term
    #    term sequence or_term ored expression rule rules""")
    #exprs = {n=>@deref(n) for n in exprnames}

    exprs ={
        "comment" => comment,
        "meaninglessness" => meaninglessness,
        "_" => _,
        "equals" => equals,
        "label" => label,
        "reference" => reference,
        "quantifier" => quantifier,
        "spaceless_literal" => spaceless_literal,
        "literal" => literal,
        "regex" => regex,
        "atom" => atom,
        "quantified" => quantified,
        "term" => term,
        "not_term" => not_term,
        "term" => term,
        "sequence" => sequence,
        "or_term" => or_term,
        "ored" => ored,
        "expression" => expression,
        "rule" => rule,
        "rules" => rules
        }

    Grammar("", exprs["rules"], exprs)
end

function _expressions_from_rules(rule_syntax::ASCIIString)
    comment = Regex("#[^\\r\\n]*", name="comment")
    meaninglessness = OneOf(Regex("\\s+"), comment, name="meaninglessness")
    _ = ZeroOrMore(meaninglessness, name="_")
    equals = Sequence(Literal("="), _, name="equals")
    label = Sequence(Regex("[a-zA-Z_][a-zA-Z_0-9]*"), _, name="label")
    reference = Sequence(label, Not(equals), name="reference")
    quantifier = Sequence(Regex("[*+?]"), _, name="quantifier")
    spaceless_literal = Regex("'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'",
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
    term = OneOf(not_term, quantified, atom, name="term")

    sequence = Sequence(term, OneOrMore(term), name="sequence")
    or_term = Sequence(Literal("/"), _, term, name="or_term")
    ored = Sequence(term, OneOrMore(or_term), name="ored")
    expression = OneOf(ored, sequence, term, name="expression")
    rule = Sequence(label, equals, expression, name="rule")
    rules = Sequence(_, OneOrMore(rule), name="rules")

    # Turn the parse tree into a map of expressions:
    rule_tree = parse(rules, rule_syntax)
    exprs, default_rule = visit(RuleVisitor(), rule_tree)
    return exprs, default_rule
end

type RuleVisitor <: NodeVisitor end

# Same as visit(v::NodeVisitor, node::Node) but here for debugging ...
function visit(v::NodeVisitor, node::Node)
    # TODO: error handling
    visit(v, node, [visit(v, n) for n in node])
end

# generic_visit
function visit(v::RuleVisitor, n::Node, visited_children)
    if !isempty(visited_children)
        return visited_children
    end
    return n
end

visit(v::RuleVisitor, n::Node{:expression}, visited_children) = visited_children[1]
visit(v::RuleVisitor, n::Node{:term}, visited_children) = visited_children[1]
visit(v::RuleVisitor, n::Node{:atom}, visited_children) = visited_children[1]

function visit(v::RuleVisitor, n::Node{:parenthesized}, visited_children)
    left_paren, _1, expression, right_paren, _2 = visited_children
    expression
end

function visit(v::RuleVisitor, n::Node{:quantifier}, visited_children)
    symbol, _ = visited_children
    symbol
end

function visit(v::RuleVisitor, n::Node{:quantified}, visited_children)
    atom, quantifier = visited_children

    quantifier = nodetext(quantifier)[1]
    if quantifier == '?'
        return Optional(atom)
    elseif quantifier == '*'
        return ZeroOrMore(atom)
    elseif quantifier == '+'
        return OneOrMore(atom)
    end
    error("How is '" * string(quantifier) * "' a quantifier to you?")
end

function visit(v::RuleVisitor, n::Node{:lookahead_term}, visited_children)
    ampersand, term, _ = visited_children
    Lookahead(term)
end

function visit(v::RuleVisitor, n::Node{:not_term}, visited_children)
    exclamation, term, _ = visited_children
    Not(term)
end

function visit(v::RuleVisitor, n::Node{:rule}, visited_children)
    label, equals, expression = visited_children

    # Apply label.label as name to copy of expression
    expression.name = label
    expression
end

function visit(v::RuleVisitor, n::Node{:sequence}, visited_children)
    term, terms = visited_children
    seq = Sequence(term, terms...)
    seq
end

function visit(v::RuleVisitor, n::Node{:ored}, visited_children)
    term, terms, = visited_children
    OneOf(term, terms...)
end

function visit(v::RuleVisitor, n::Node{:or_term}, visited_children)
    slash, _, term = visited_children
    term
end

function visit(v::RuleVisitor, n::Node{:label}, visited_children)
    name, _ = visited_children
    nodetext(name)
end

function visit(v::RuleVisitor, n::Node{:reference}, visited_children)
    label, not_equals = visited_children
    LazyReference(label)
end

function visit(v::RuleVisitor, n::Node{:regex}, visited_children)
    tilde, literal, flags, _ = visited_children
    pattern = literal.literal
    flags = lowercase(nodetext(flags))
    Expressions.Regex(pattern, options=flags)
end

function visit(v::RuleVisitor, n::Node{:literal}, visited_children)
    spaceless_literal, _ = visited_children
    spaceless_literal
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
    # TODO: The lazy references are packing themselves into arrays, I don't know why
    if isa(expr, Expression)
        exprname = expr.name
    elseif isa(expr, Node)
        exprname = name(expr) # expr is a node
    else
        @assert 0
    end
    if !isempty(exprname) && !in(exprname, unwalked_names)
        println("NOT WALKING")
        # unwalked_names started out with all the rule names in it, so, if
        # this is a named expr and it isn't in there, it must have been
        # resolved.
        return rule_map[exprname]
    # If not, resolve it:
    elseif isa(expr, LazyReference)
        label = unicode(expr)
        if !in(label, walking_names)
            # We aren't already working on traversing this label:
            local reffed_expr
            try
                reffed_expr = rule_map[label]
            #except KeyError:
            catch e
                if isa(e, KeyError)
                    # TODO: UndefinedLabel type
                    # throw(UndefinedLabel(expr))
                    @show rule_map
                    error("Undefined Label ", expr)
                end
                rethrow(e)
            end
            rule_map[label] = _resolve_refs(rule_map, reffed_expr, unwalked_names, Set(walking_names..., label))

            # If we recurse into a compound expression, the remove()
            # happens in there. But if this label points to a non-compound
            # expression like a literal or a regex or another lazy
            # reference, we need to do this here:
            delete!(unwalked_names, label)
        end
        return rule_map[label]
    else
        members = []
        try
            members = getfield(expr, :members)
        catch
            println("no members")
        end
        if !isempty(members)
            expr.members = [_resolve_refs(rule_map, m, unwalked_names, walking_names) for m in members]
        end
        if exprname != ""
            delete!(unwalked_names, exprname)
        end
        return expr
    end
end

function pause()
    println("PAUSED")
    readline()
end

# return dictionary of expressions and a rule name
function visit(v::RuleVisitor, n::Node{:rules}, visited_children)
    _, rules = visited_children

    rule_map = {expr.name => expr for expr in rules}
    unwalked_names = Set(collect(keys(rule_map))...)
    while !isempty(unwalked_names)
        rule_name = first(unwalked_names)
        rule_map[rule_name] = _resolve_refs(rule_map, rule_map[rule_name], unwalked_names, Set(rule_name))
        delete!(unwalked_names, rule_name)
    end

    return rule_map, rules[1]
end

# TODO: Need a function for escaping \n, \t etc
# TODO: unicode
function visit(v::RuleVisitor, n::Node{:spaceless_literal}, visited_children)
    # strip the single quotes
    Literal(escape_string(nodetext(n)[2:end-1]))
end

#rule_grammar = Grammar()  # Bootstrapping
#rule_grammar = Grammar(rule_grammar, rule_syntax)  # Level 2
#@show rule_grammar
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

#_ = Regex("\\s*", name="_")
#lc = Regex("[a-z]+", name="lc")
#lab = Sequence(lc, _)
#equals = Sequence(Literal("="), _, name="equals")
#rule = Sequence(lab, equals, lab, name="rule")
#rules = Sequence(_, OneOrMore(rule), name="rules")
#mytext = " rul=asdf foo=bsdf bar=csdf"
#node = parse(rules, mytext)
## rules, default_rule = visit(RuleVisitor(), node)
#@show typeof(node)
#for visitret in visit(RuleVisitor(), node)
#    @show visitret
#end
#rules, default_rule = visit(RuleVisitor(), node)
#@show rules, default_rule
#@test is(default_rule, ASCIIString)
#@test is(rules, Dict)


end
