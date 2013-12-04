
module Grammars

export Grammar

import Expressions: Literal, Regex, Sequence, OneOf, Lookahead, Optional, ZeroOrMore, OneOrMore, Not, Expression, parse
import Expressions
# NodeVisitor is abstract and it tells me I can't import it
import Nodes: Node, NodeVisitor, pprettily, text, name, lift_child

type Grammar
    rules::ASCIIString
    default_rule::Expression
    exprs::Dict
end

function Grammar(rule_grammar::Grammar, rule_syntax::ASCIIString; default_rule=nothing)
    @show rule_grammar.default_rule
    @show rule_grammar.exprs
    @show rule_grammar.rules
    exprs, first_rule = _expressions_from_rules(rule_grammar, rule_syntax)
    println("\n----------------------------------------")
    @show first_rule
    @show default_rule
    default_rule = is(default_rule,nothing) ? default_rule : first_rule.name
    @show default_rule
    Grammar(rule_syntax, default_rule, exprs)
end

function Grammar(rule_syntax::ASCIIString)
    Grammar(rule_grammar, rule_syntax)
end

function parse(grammar::Grammar, text::ASCIIString; pos=1)
    @show "parse grammar", grammar, text, pos
    # TODO Expressions.parse to support pos parameter
    parse(grammar.default_rule, text)
end

function _expressions_from_rules(grammar::Grammar, rules::ASCIIString)
    @show "soon to parse grammar", grammar, rules
    tree = parse(grammar, rules)
    return visit(RuleVisitor(), tree)
end

function Grammar() # Bootstrapping
    exprs, default_rule = _expressions_from_rules(rule_syntax)
    @show exprs
    @show default_rule
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
    spaceless_literal = ~'u?r?\\'[^\\'\\\\\\\\]*(?:\\\\\\\\.[^\\'\\\\\\\\]*)*\\''is / ~'u?r?"[^"\\\\\\\\]*(?:\\\\\\\\.[^"\\\\\\\\]*)*"'is

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
    exprs, default_rule = visit_all(RuleVisitor(), rule_tree)
    return exprs, default_rule
end

type RuleVisitor <: NodeVisitor end

# Same as visit(v::NodeVisitor, node::Node) but here for debugging ...
function visit_all(v::NodeVisitor, node::Node)
    visited_children = [visit_all(v, child) for child in node]

    try
        return visit(v, node, visited_children...)
    catch e
        @show node
        @show visited_children
        @show v

        rethrow(e)

        #println(string(e))
        #if isa(e, MethodError) && match(r"MethodError.visit,", string(e)) != nothing
        #    # generic visit
        #    if isempty(visited_children)
        #        return node
        #    end
        #    return visited_children
        #else
        #    rethrow(e)
        #end
    end
end

visit(v::RuleVisitor, n::Node{:comment}) = n
visit(v::RuleVisitor, n::Node{:meaninglessness}, children...) = children
visit(v::RuleVisitor, n::Node{:__parsimonious_no_name__}) = n
visit(v::RuleVisitor, n::Node{:__parsimonious_no_name__}, children...) = children
visit(v::RuleVisitor, n::Node{:_}) = n
visit(v::RuleVisitor, n::Node{:_}, children...) = children
visit(v::RuleVisitor, n::Node{:equals}) = n
visit(v::RuleVisitor, n::Node{:equals}, children...) = children

visit(v::RuleVisitor, n::Node{:expression}, child) = lift_child(v, n, child)
visit(v::RuleVisitor, n::Node{:term}, child) = lift_child(v, n, child)
visit(v::RuleVisitor, n::Node{:atom}, child) = lift_child(v, n, child)

function visit(v::RuleVisitor, n::Node{:parenthesized}, left_paren, _1, expression, right_paren, _2)
    @show "visit parenthesized"
    pprettily(n)
    expression
end

function visit(v::RuleVisitor, n::Node{:quantifier}, symbol, _)
    @show "visit quantifier", symbol
    pprettily(n)
    symbol
end

function visit(v::RuleVisitor, n::Node{:quantified}, atom, quantifier)
    @show "visit quantified", quantifier
    pprettily(n)
    quantifier = text(quantifier)[1]
    if quantifier == '?'
        return Optional(atom)
    elseif quantifier == '*'
        return ZeroOrMore(atom)
    elseif quantifier == '+'
        return OneOrMore(atom)
    end
    error("How is '" * string(quantifier) * "' a quantifier to you?")
end

function visit(v::RuleVisitor, n::Node{:lookahead_term}, ampersand, term, _)
    @show "visit lookahead_term"
    pprettily(n)
    Lookahead(term)
end

function visit(v::RuleVisitor, n::Node{:not_term}, exclamation, term, _)
    @show "visit not_term"
    pprettily(n)
    Not(term)
end

function visit(v::RuleVisitor, n::Node{:rule}, label, equals, expression)
    @show "visit rule"
    pprettily(n)

    println("HELLO LABEL=EXPRESSION")
    @show label
    @show equals
    @show expression
    expression.name = label
    return expression
end

function visit(v::RuleVisitor, n::Node{:sequence}, term, terms)
    @show "visit sequence"
    pprettily(n)

    seq = Sequence(term, terms...)
    @show seq
    return seq
end

function visit(v::RuleVisitor, n::Node{:ored}, term, terms)
    @show "visit ored"
    pprettily(n)
    OneOf(term, terms...)
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

type LazyReference <: Expression name end

_as_rhs(lr::LazyReference) = "<LazyReference to $(lr.name)>"

function visit(v::RuleVisitor, n::Node{:reference}, label, not_equals)
    @show "visit label"
    pprettily(n)
    LazyReference(label)
end

function visit(v::RuleVisitor, n::Node{:regex}, tilde, literal, flags, _)
    @show "visit regex"
    pprettily(n)
    @show literal
    Expressions.Regex(literal, options=text(flags))
end

visit(v::RuleVisitor, n::Node{:literal}, literal, spaceless_literal, _) = spaceless_literal

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
    @show expr
    # TODO: The lazy references are packing themselves into arrays, I don't know why
    if isa(expr, Expression)
        exprname = expr.name
    else
        exprname = name(expr) # expr is a node
    end
    if !isempty(exprname) && !in(exprname, unwalked_names)
        println("NOT WALKING")
        # unwalked_names started out with all the rule names in it, so, if
        # this is a named expr and it isn't in there, it must have been
        # resolved.
        return rule_map[exprname]
    # If not, resolve it:
    elseif isa(expr, LazyReference)
        label = expr.name
        if !in(label, walking_names)
            # We aren't already working on traversing this label:
            local reffed_expr
            try
                reffed_expr = rule_map[label]
            #except KeyError:
            catch e
                if isa(e, KeyError)
                    # TODO: UndefinedLabel type
                    throw(UndefinedLabel(expr))
                end
                rethrow(e)
            end
            rule_map[label] = _resolve_refs( rule_map, reffed_expr, unwalked_names, tuple(walking_names..., label))

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
            @show members
        catch
            println("no members")
        end
        @show !isempty(members)
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
function visit(v::RuleVisitor, n::Node{:rules}, _, rules)
    println("VISIT RULES")
    for rule in rules
        @show(rule.name, rule)
    end

    rule_map = {expr.name => expr for expr in rules}
    unwalked_names = Set(collect(keys(rule_map))...)
    while !isempty(unwalked_names)
        rule_name = first(unwalked_names)
        @show "walk", rule_name, rule_map[rule_name]
        rule_map[rule_name] = _resolve_refs(rule_map, rule_map[rule_name], unwalked_names, (rule_name,))
        delete!(unwalked_names, rule_name)
    end

    @show rules[1]
    return rule_map, rules[1]
end

# TODO: Need a function for escaping \n, \t etc
# TODO: unicode
function visit(v::RuleVisitor, n::Node{:spaceless_literal}, spaceless_literal, visited_children...)
    @show spaceless_literal
    quit()
    Literal(escape_string(text(spaceless_literal)))
end

function visit(v::RuleVisitor, n::Node{:spaceless_literal}, spaceless_literal)
    @show n
    @show spaceless_literal
    quit()
    Literal(escape_string(text(spaceless_literal)))
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
