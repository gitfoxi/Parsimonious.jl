
module Grammars

using Util
using Nodes
using Expressions
import Nodes.visit # for overloading NodeVisitor

export Grammar, lookup, rule_grammar, parse, unicode, match

# You have to import everything you want to overload and export
import Expressions: parse, unicode, match

type Grammar
    rules::String
    default_rule::Expression
    exprs::Dict
end

# TODO: Grammar <: Associative
# for now, a lookup method
lookup(g::Grammar, key) = g.exprs[key]

# As an interface, calling with similar parameters in a different order could
# not be more fucking confusing. TODO: sort it out
function Grammar(rule_grammar::Grammar, rule_syntax::String;
    default_rule=nothing)
    exprs, first_rule = _expressions_from_rules(rule_grammar, rule_syntax)
    default_rule = is(default_rule,nothing) ? first_rule : exprs[default_rule]
    Grammar(rule_syntax, default_rule, exprs)
end

Grammar(rule_syntax::String) = Grammar(rule_grammar, rule_syntax)

function Grammar() # Bootstrapping
    exprs, default_rule = _expressions_from_rules(rule_syntax)
    Grammar(rule_syntax, default_rule, exprs)
end

parse(grammar::Grammar, text::String, pos=1) = parse(grammar.default_rule, text, pos)
match(grammar::Grammar, text::String, pos=1) = match(grammar.default_rule, text, pos)

rule_syntax = p"""
    # Ignored things (represented by _) are typically hung off the end of the
    # leafmost kinds of nodes. Literals like "/" count as leaves.

    rules = _ rule+
    rule = label equals expression
    equals = "=" _
    literal = spaceless_literal _

    # So you can't spell a regex like `~"..." ilm`:
    """ * """ # TODO: barf backslash handling
    spaceless_literal = ~"\\"[^\\"\\\\]*(?:\\\\.[^\\"\\\\]*)*\\""is /
                        ~"'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'"is
    """ * p""" # /barf
    expression = ored / sequence / term
    or_term = "/" _ term
    ored = term or_term+
    sequence = term term+
    not_term = "!" term _
    lookahead_term = "&" term _
    term = not_term / lookahead_term / quantified / atom
    quantified = atom quantifier
    atom = reference / literal / regex / parenthesized
    regex = "~" spaceless_literal ~"[ilmsux]*"i _
    parenthesized = "(" _ expression ")" _
    quantifier = ~"[*+?]" _
    reference = label !equals

    # A subsequent equal sign is the only thing that distinguishes a label
    # (which begins a new rule) from a reference (which is just a pointer to a
    # rule defined somewhere else):
    label = ~"[a-zA-Z_][a-zA-Z_0-9]*" _

    # _ = ~"\s*(?:#[^\r\n]*)?\s*"
    _ = meaninglessness*
    meaninglessness = ~"\s+" / comment
    comment = ~"#[^\r\n]*"
"""

function boot_expressions()
    # Return a stripped-down rules which may be used to parse
    # rule_syntax to generate the complete rule_grammar
    comment = Regex(p"#[^\r\n]*", name="comment")
    meaninglessness = OneOf(Regex(p"\s+"), comment, name="meaninglessness")
    _ = ZeroOrMore(meaninglessness, name="_")
    equals = Sequence(Literal("="), _, name="equals")
    label = Sequence(Regex("[a-zA-Z_][a-zA-Z_0-9]*"), _, name="label")
    reference = Sequence(label, Not(equals), name="reference")
    quantifier = Sequence(Regex("[*+?]"), _, name="quantifier")
    spaceless_literal = Regex(p""" "[^"\\]*(?:\\.[^"\\]*)*" """[2:end-1],
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
end

function boot_grammar()
    exprs = boot_expressions()
    Grammar("", exprs["rules"], exprs)
end

function _expressions_from_rules(grammar::Grammar, rules::String)
    tree = parse(grammar, rules)
    visit(RuleVisitor(), rules, tree)
end

# make boot grammar
function _expressions_from_rules(rule_syntax::String)
    rules = boot_expressions()["rules"]
    rule_tree = parse(rules, rule_syntax)
    exprs, default_rule = visit(RuleVisitor(), rule_syntax, rule_tree)
    return exprs, default_rule
end

type RuleVisitor <: NodeVisitor end

# generic_visit
visit(v::RuleVisitor, f::String, n::LeafNode) = n
visit(v::RuleVisitor, f::String, n::ParentNode, visited_children) = visited_children

# TODO: tuples not arrays for visited_children
visit(v::RuleVisitor, f::String, n::ParentNode{:expression}, visited_children) = visited_children[1]
visit(v::RuleVisitor, f::String, n::ParentNode{:term}, visited_children) = visited_children[1]
visit(v::RuleVisitor, f::String, n::ParentNode{:atom}, visited_children) = visited_children[1]

function visit(v::RuleVisitor, f::String, n::ParentNode{:parenthesized}, visited_children)
    left_paren, _1, expression, right_paren, _2 = visited_children
    expression
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:quantifier}, visited_children)
    symbol, _ = visited_children
    symbol
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:quantified}, visited_children)
    atom, quantifier = visited_children
    quantifier = nodetext(quantifier, f)[1]
    if quantifier == '?'
        return Optional(atom)
    elseif quantifier == '*'
        return ZeroOrMore(atom)
    elseif quantifier == '+'
        return OneOrMore(atom)
    end
    error("How is '" * string(quantifier) * "' a quantifier to you?")
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:lookahead_term}, visited_children)
    ampersand, term, _ = visited_children
    Lookahead(term)
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:not_term}, visited_children)
    exclamation, term, _ = visited_children
    Not(term)
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:rule}, visited_children)
    label, equals, expression = visited_children
    expression.name = label
    expression
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:sequence}, visited_children)
    term, terms = visited_children
    Sequence(term, terms...)
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:ored}, visited_children)
    term, terms, = visited_children
    OneOf(term, terms...)
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:or_term}, visited_children)
    slash, _, term = visited_children
    term
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:label}, visited_children)
    name, _ = visited_children
    nodetext(name, f)
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:reference}, visited_children)
    label, not_equals = visited_children
    LazyReference("", label)
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:regex}, visited_children)
    tilde, literal, flags, _ = visited_children
    pattern = literal.literal
    flags = lowercase(nodetext(flags, f))
    Expressions.Regex(pattern, options=flags)
end

function visit(v::RuleVisitor, f::String, n::ParentNode{:literal}, visited_children)
    spaceless_literal, _ = visited_children
    spaceless_literal
end

type UndefinedLabel <: Exception label end

showerror(io::IO, e::UndefinedLabel) = print(io,"The label $(e.label) was never defined.")

function _resolve_refs(rule_map, expr, unwalked_names, walking_names)
    #Return an expression with all its lazy references recursively
    #resolved.

    #Resolve any lazy references in the expression ``expr``, recursing into
    #all subexpressions. Populate ``rule_map`` with any other rules (named
    #expressions) resolved along the way. Remove from ``unwalked_names`` any
    #which were resolved.

    #:arg walking_names: The stack of labels we are currently recursing
    #    through. This prevents infinite recursion for circular refs.

    # If it's a top-level (named) expression and we've already walked it,
    # don't walk it again:
    exprname = expr.name
    if !isempty(exprname) && !in(exprname, unwalked_names)
        # unwalked_names started out with all the rule names in it, so, if
        # this is a named expr and it isn't in there, it must have been
        # resolved.
        return rule_map[exprname]
    # If not, resolve it:
    elseif isa(expr, LazyReference)
        label = expr.label
        if !in(label, walking_names)
            # We aren't already working on traversing this label:
            local reffed_expr
            try
                reffed_expr = rule_map[label]
            catch e
                if isa(e, KeyError)
                    # println(rule_map)
                    throw(UndefinedLabel(expr))
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

# return dictionary of expressions and a rule name
function visit(v::RuleVisitor, f::String, n::ParentNode{:rules}, visited_children)
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

visit(v::RuleVisitor, f::String, n::ParentNode{:spaceless_literal}, _) = Literal(nodetext(n, f)[2:end-1])
visit(v::RuleVisitor, f::String, n::ChildlessNode{:spaceless_literal}) = Literal(nodetext(n, f)[2:end-1])

rule_grammar = Grammar()  # Level 1 -- Bootstrap grammar parses rule_symtax
rule_grammar = Grammar(rule_grammar, rule_syntax)  # Level 2 -- generated rule_grammar parses rule_syntax
rule_grammar = Grammar(rule_grammar, rule_syntax)  # Level 3 -- re-generated rule_grammar parses rule_syntax

# TODO: Stupid name
function unicode(g::Grammar)
    """Return a rule string that, when passed to the constructor, would
    reconstitute the grammar."""
# TODO: This got hacked up in debugging. It was prettier to start.
    sexprs = [as_rule(g.default_rule)]
    for expr in g.exprs
        if !is(expr[2], g.default_rule)
            push!(sexprs, as_rule(expr[2]))
        end
    end
    join([sexprs], "\n")
end

end # module
