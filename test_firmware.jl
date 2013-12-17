
module test_firmware

using Grammars
using Nodes
using Util
# TODO: How to have less import stuff?
import Grammars.visit
import Nodes.visit

type FirmwareVisitor <: NodeVisitor end


type FirmwareCommand
    command::String
    isquery::Bool
    parameters
end

FirmwareCommand("FTST",true,[])

# TODO: RulesVisitor and test visitors to adopt new vararg visitor technolgy
function visit_on_the_way_down2(v::NodeVisitor, node::ParentNode)
    return [govisit(v, n) for n in node]
end

function visit_on_the_way_down2(v::NodeVisitor, node::ParentNode; debug=false)
    return [govisit(v, n; debug=debug) for n in node]
end

# No children
visit_on_the_way_down2(v::NodeVisitor, node::LeafNode; debug=false) = []
visit_on_the_way_down2(v::NodeVisitor, node::LeafNode) = []

# TODO: better debug mode that prints:
#   parse tree  --  rule --  visit(visitor, node, param, param, ...) -- return value
# The tree can be upside down in Visitor order
# Auto-spacing based on column matched.
# Then you can just accept the default visit rules and go through
# the debug to figure out how to process things
#
function govisit(v::NodeVisitor, node::MatchNode; debug=false)
    if debug
        visited_children = visit_on_the_way_down2(v, node; debug=true)
    else
        visited_children = visit_on_the_way_down2(v, node)
    end
    visited_children = filter(x -> x != nothing, visited_children)
    try
        if debug
            println("--------------------------------------")
            which(visit2, v, node, visited_children...)
            @show name(node)
            @show visited_children
            returns = visit2(v, node, visited_children...)
            @show returns
            println("--------------------------------------")
            return returns
        else
            return visit2(v, node, visited_children...)
        end
    catch e
        if isa(e, VisitationError)
            rethrow(e)
        else
            rethrow(VisitationError(node, e))
        end
    end
end

type FwVis <: NodeVisitor end
visit2(::FwVis, n::ParentNode{:isquery}, questionmark) = questionmark == "?"
visit2(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool) = FirmwareCommand(command, isquery, [])
visit2(::FwVis, n::LeafNode) = nodetext(n)
visit2(::FwVis, n::ParentNode, visited_children...) = visited_children

function tryit(txt)
    tree = parse(g, txt)
    println(tree)
    println(govisit(FwVis(), tree; debug=true))
end

g = grammar"""
    statement = command isquery
    command = ~"\w{4}"
    isquery = "?"?
    """

txt = "FTST?"
tree = parse(g, txt)

tryit("FTST?")

# With out the '?' isquery is a leaf
visit2(::FwVis, n::LeafNode{:isquery}) = false

tryit("FTST")
#@show visit2(FwVis(), txt, tree; debug=true)

g = grammar"""
    statements = (statement termination)+
    statement = command isquery
    command = ~"\w{4}"
    isquery = "?"?
    termination = ~'[;\n]'
    """

function fwtest(name, grammar, text)
    tree = parse(g.exprs[name], text)
    govisit(FwVis(), tree; debug=true)
end

import Base.isequal
function isequal(a::FirmwareCommand, b::FirmwareCommand)
    a.command == b.command &&
    a.isquery == b.isquery &&
    a.parameters == b.parameters
end

using Base.Test
@test fwtest("statement", g, "FTST?") == FirmwareCommand("FTST",true,[])
@test fwtest("statement", g, "FTST") == FirmwareCommand("FTST",false,[])


tree = parse(g.exprs["statement"], "FTST?")
@show govisit(FwVis(), tree)
tree = parse(g.exprs["statement"], "FTST")
@show govisit(FwVis(), tree)

visit2(::FwVis, n::ParentNode{:statements}, statements...) = [statement_term[1] for statement_term in statements]
@show fwtest("statements", g, "FTST?;FTST?;FTST?\nFTST?\nFTST;FTST\n")

g = grammar"""
    statements = (statement termination)+
    statement = command isquery params
    command = ~"\w{4}"
    isquery = "?"?
    termination = ~'[;\n]'
    params = more_params / one_param / no_params
    even_more_params = (comma param)+
    comma = ','
    more_params = somespace param even_more_params
    one_param = somespace param
    param = ~'[^ \t,;\n]*'
    no_params = ''
    somespace = ~'[ \t]+'
    """

tree = parse(g, "ASDF;ASDF 1;ASDF 1,2;ASDF one;ASDF one,two;")
println(tree)

visit2(::FwVis, n::LeafNode{:somespace}) = nothing
@show fwtest("statements", g,  "ASDF;ASDF 1;ASDF 1,2;ASDF one;ASDF one,two;")

visit2(::FwVis, n::LeafNode{:comma}) = nothing
visit2(::FwVis, n::ParentNode{:even_more_params}, boxes) = [box for box in boxes]
visit2(::FwVis, n::ParentNode{:more_params}, param, even_more_params) = vcat([param], even_more_params)
visit2(::FwVis, n::ParentNode{:one_param}, param) = [param]
visit2(::FwVis, n::LeafNode{:no_params}) = []
visit2(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool, params) = FirmwareCommand(command, isquery, params[1])

@show fwtest("statements", g,  "ASDF;ASDF 1;ASDF 1,2;ASDF one;ASDF one,two;")
@test fwtest("statements", g,  "ASDF;ASDF 1;ASDF 1,2;ASDF one;ASDF one,two;") == 
        {FirmwareCommand("ASDF",false,[]),FirmwareCommand("ASDF",false,["1"]),FirmwareCommand("ASDF",false,{"1","2"}),FirmwareCommand("ASDF",false,["one"]),FirmwareCommand("ASDF",false,{"one","two"})}

g = grammar"""
    statements = (statement termination)+
    statement = command isquery (somespace params)?
    command = ~"\w{4}"
    isquery = "?"?
    termination = ~'[;\n]'
    params = more_params / one_param / no_params
    even_more_params = (comma param)+
    comma = ','
    more_params = param even_more_params
    one_param = param
    no_params = ''
    somespace = ~'[ \t]+'
    param = quoted / list / bare
    bare = ~'[^ \t,;\n")(]*'
    dq = '"'
    quoted = dq ~'[^"]*' dq
    open_paren = '('
    close_paren = ')'
    list = open_paren params close_paren
    """

# still works?
# Now param has to be reboxed
visit2(::FwVis, n::ParentNode{:topparams}, params...) = params
visit2(::FwVis, n::ParentNode{:param}, param...) = param[1]
println(parse(g, "ASDF;ASDF 1;ASDF 1,2;ASDF one;ASDF one,two;"))
@show fwtest("statements", g,  "ASDF;ASDF 1;ASDF 1,2;ASDF one;ASDF one,two;")
@test fwtest("statements", g,  "ASDF;ASDF 1;ASDF 1,2;ASDF one;ASDF one,two;") == 
        {FirmwareCommand("ASDF",false,[]),FirmwareCommand("ASDF",false,["1"]),FirmwareCommand("ASDF",false,{"1","2"}),FirmwareCommand("ASDF",false,["one"]),FirmwareCommand("ASDF",false,{"one","two"})}

@show fwtest("statements", g, """ASDF one,"two",(),(one),(one,"two"),(one,("two",(three)));""")

pause("Paused")

sample_command="FTST?"
# TODO: should be:
# command_grammar=Grammar(p"command = ~'\w{4}' '?'")
# rule_grammar is implicit
command_grammar=Grammar(rule_grammar, p"command = ~'\w{4}' '?'")
tree = parse(command_grammar, sample_command)
# tree = parse(command_grammar, sample_command)

sample_text =
"""

    FTST?
    FTST P
    SHIT 12,10;
    SHIT "poo poo"
    SHIT ()
    SQGB STSQ,30,(P2)
    SQGB? (@@);
    DFPN 10101,"1",(Clock)
    DFPN? (@)
    DFPN (11901, 11902),,(vee)

"""

# TODO: Print the partial parse tree on failure to match all of the text

# TODO: BUG: noparams has to be defined like this:
#    noparams = ~"[ \\t]*"
# If I define it like I want to then noparams doesn't appear in the parse tree
#    noparams = _

firmware_grammar_spec =
"""

    statements = statement+ whitespace
    statement = whitespace command isquery params termination
    command = ~"\\w{4}"
    more_params = (',' _ param)+
    paramlist = nonoptspace param _ more_params
    oneparam = nonoptspace nonemptyparam
    noparams = ~"[ \\t]*"
    params = paramlist / oneparam / noparams
    param = litparam / quotedparam / parenthesizedparam / emptyparam
    nonemptyparam = litparam / quotedparam / parenthesizedparam
    litparam = ~'[^")(;, \\t\\n]+'
    quotedparam = '"' ~'[^"]*' '"'
    parenthesizedparam = '(' params ')'
    emptyparam = _   # not so sure about this one
    termination = _ ~'[;\\n]'
    isquery = '?'?
    whitespace = ~"[ \\t\\n]*"
    _ = ~"[ \\t]*"
    nonoptspace = ~"[ \\t]+"

"""
# won't match:    termination = linespace (';' / '\\n')  # optional semicolon? seriously?

firmware_grammar = Grammar(rule_grammar, firmware_grammar_spec)
g = firmware_grammar

parse(g.exprs["command"], "FTST")
parse(g.exprs["statement"], "FTST;")
parse(g.exprs["termination"], ";")
parse(g.exprs["termination"], "\n")
# parse(g.exprs["simple_statement"], "FTST\n")
parse(g.exprs["statement"], "FTST\n")
parse(g.exprs["statement"], "FTST?;")
parse(g.exprs["statement"], "FTST P\n")
parse(g.exprs["statement"], "FTST P;")
parse(g.exprs["litparam"], "P")

# sample_text = "FTST?\n"
tree = parse(firmware_grammar, sample_text)
pprettily(tree)

# TODO: Doctest

# TODO: Refactor: LeafNode -> LeafNode
# TODO: Trace visitor

function visit(::FirmwareVisitor, n::ParentNode{:statement}, visited_children)
    _, command, isquery, parameters, _ = visited_children
    FirmwareCommand(command, isquery, parameters)
end

visit(::FirmwareVisitor, n::ParentNode{:isquery}, visited_children) = visited_children[1] == "?"
visit(::FirmwareVisitor, n::LeafNode{:isquery}) = false

function visit(::FirmwareVisitor, n::ParentNode{:paramlist}, visited_children)
    _, param, _, more_params = visited_children
    @show param more_params
    vcat([param], more_params)
end

function visit(::FirmwareVisitor, n::ParentNode{:more_params}, visited_children)
    [vc[3] for vc in visited_children]
end

visit(::FirmwareVisitor, n::ParentNode{:param}, visited_children) = visited_children[1]
visit(::FirmwareVisitor, n::ParentNode{:nonemptyparam}, visited_children) = visited_children[1]
visit(::FirmwareVisitor, n::ParentNode{:statements}, visited_children) = visited_children[1]
visit(::FirmwareVisitor, n::ParentNode{:params}, visited_children) = visited_children[1]
visit(::FirmwareVisitor, n::LeafNode{:noparams}) = []
visit(::FirmwareVisitor, n::ParentNode{:oneparam}, visited_children) = [visited_children[2]]
visit(::FirmwareVisitor, n::ParentNode{:litparam}, visited_children) = visited_children[1]
visit(::FirmwareVisitor, n::ParentNode{:quotedparam}, visited_children) = visited_children[2]
visit(::FirmwareVisitor, n::ParentNode{:parenthesizedparam}, visited_children) = visited_children[2]
visit(::FirmwareVisitor, n::ParentNode{:emptyparam}, visited_children) = []

visit(::FirmwareVisitor, n::ParentNode, visited_children) = visited_children
visit(::FirmwareVisitor, n::LeafNode, visited_children) = nodetext(n)
visit(::FirmwareVisitor, n::LeafNode) = nodetext(n)

firmware_statements = visit(FirmwareVisitor(), sample_text, tree)
# TODO: make keyword arguments work right
# firmware_statements = visit(FirmwareVisitor(), sample_text, tree; debug=true)

@show firmware_statements

end
