
module test_firmware

using Grammars
using Nodes
using Util
# TODO: How to have less import stuff?
import Grammars.visit
import Nodes.visit

type FirmwareVisitor <: NodeVisitor end


g = grammar"""
    statement = command isquery
    command = ~"\w{4}"
    isquery = "?"?
    """

type FirmwareCommand
    command::ASCIIString
    isquery::Bool
    parameters
end

txt = "FTST?"
@show tree = parse(g, txt)
pprettily(tree, txt)

# new vararg visitor technolgy
function visit_on_the_way_down2(v::NodeVisitor, fulltext::String, node::ParentNode)
    return [go_visit(v, fulltext, n) for n in node]
end

# No children
visit_on_the_way_down2(v::NodeVisitor, fulltext::String, node::LeafNode) = []

function go_visit(v::NodeVisitor, fulltext::String, node::MatchNode)
    visited_children = visit_on_the_way_down2(v, fulltext, node)
    try
        # TODO: Move this whole thing to tuples -- but not now
        @show v
        @show name(node)
        @show visited_children
        @which visit2(v, fulltext, node, visited_children)
        returns = visit2(v, fulltext, node, visited_children...)
        @show returns
        return returns
    catch e
        if isa(e, VisitationError)
            rethrow(e)
        else
            rethrow(VisitationError(node, fulltext, e))
        end
    end
end


type FwVis <: NodeVisitor end
visit2(::FwVis, txt::String, n::ParentNode{:isquery}, questionmark) = questionmark == "?"
visit2(::FwVis, txt::String, n::ParentNode{:statement}, command::String, isquery::Bool) = FirmwareCommand(command, isquery, [])
visit2(::FwVis, txt::String, n::LeafNode) = nodetext(n, txt)
visit2(::FwVis, txt::String, n::ParentNode, visited_children) = visited_children
@show fw = go_visit(FwVis(), txt, tree)
#@show visit2(FwVis(), txt, tree; debug=true)

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
    termination = _ ~'[;\\n]'  # optional semicolon? seriously?
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
pprettily(tree, sample_text)

# TODO: Doctest

# TODO: Refactor: LeafNode -> LeafNode
# TODO: Trace visitor

function visit(::FirmwareVisitor, f, n::ParentNode{:statement}, visited_children)
    _, command, isquery, parameters, _ = visited_children
    FirmwareCommand(command, isquery, parameters)
end

visit(::FirmwareVisitor, f::String, n::ParentNode{:isquery}, visited_children) = visited_children[1] == "?"
visit(::FirmwareVisitor, f::String, n::LeafNode{:isquery}) = false

function visit(::FirmwareVisitor, f, n::ParentNode{:paramlist}, visited_children)
    _, param, _, more_params = visited_children
    @show param more_params
    vcat([param], more_params)
end

function visit(::FirmwareVisitor, f, n::ParentNode{:more_params}, visited_children)
    [vc[3] for vc in visited_children]
end

visit(::FirmwareVisitor, f, n::ParentNode{:param}, visited_children) = visited_children[1]
visit(::FirmwareVisitor, f, n::ParentNode{:nonemptyparam}, visited_children) = visited_children[1]
visit(::FirmwareVisitor, f, n::ParentNode{:statements}, visited_children) = visited_children[1]
visit(::FirmwareVisitor, f, n::ParentNode{:params}, visited_children) = visited_children[1]
visit(::FirmwareVisitor, f::String, n::LeafNode{:noparams}) = []
visit(::FirmwareVisitor, f, n::ParentNode{:oneparam}, visited_children) = [visited_children[2]]
visit(::FirmwareVisitor, f, n::ParentNode{:litparam}, visited_children) = visited_children[1]
visit(::FirmwareVisitor, f, n::ParentNode{:quotedparam}, visited_children) = visited_children[2]
visit(::FirmwareVisitor, f, n::ParentNode{:parenthesizedparam}, visited_children) = visited_children[2]
visit(::FirmwareVisitor, f, n::ParentNode{:emptyparam}, visited_children) = []

visit(::FirmwareVisitor, f, n::ParentNode, visited_children) = visited_children
visit(::FirmwareVisitor, f, n::LeafNode, visited_children) = nodetext(n, f)
visit(::FirmwareVisitor, f::String, n::LeafNode) = nodetext(n, f)

firmware_statements = visit(FirmwareVisitor(), sample_text, tree)
# TODO: make keyword arguments work right
# firmware_statements = visit(FirmwareVisitor(), sample_text, tree; debug=true)

@show firmware_statements

end
