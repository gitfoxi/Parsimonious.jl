
module test_firmware

# md""" -- markdown general discussion
macro md_mstr(s) println(s) end
# qc""" -- quote code -- run and print code -- don't print result
macro qc_mstr(s)
    quote
        println("```jl\n", $s, "```")
        $(esc(Base.parse("begin " * s * " end")))
    end
end

# qcr""" -- quote code repl -- run and print code and result like it was run in
# repl
macro qcr_mstr(s)
    quote
        oldstdout = STDOUT
        rd, wr = redirect_stdout()
        value = $(esc(Base.parse(s)))
        println()
        ioout = readavailable(rd)
        redirect_stdout(oldstdout)
        print("```jl\n",
            replace(strip($s), r"^"m, "julia> "), "\n",
            rstrip(repr(value) * "\n" * ioout))
        println("```")
    end
end

# TODO: qcd(command, test_return, test_stdout) -- doctest the command too.

md"""
Parsimonious.jl
===============

PSA: When you learn that it is fun and easy to write parsers you will be
tempted to invent a complex, heirarchical data format -- or worse a DSL.  Some
dude on Reddit told me DSL stands for Dog Shit Language. As a professional who
both works with DSLs and picks up dog shit on reg, I must say I find the
comparison a little unfair to dog shit. The answer is almost always JSON, XML,
[config] or CSV for data and some established language for whatever you are
trying to express. 

If you're writing parsers to rid the world of entrenched DSLs or arbitrary data
formats then you're doing it right. Thank you for your attention.
"""

qc"""
using Parsimonious
import Parsimonious.visit
"""
# TODO: How to have less import stuff?

md"""
Sorry about `import Parsimonious.visit`. We're going to overload that function
    shortly but we also want some default functionality.

The key is to remain calm and organized at all times. Otherwise parser
development will get away from you. Our Firmware Command DSL was designed by
some fuckwit to communicate with an HP 93k tester. It looks like this:

    FTST?
    FTST P
    SREC? (@)
    SREC ACT,(@)
    SVLR TIM,PRM,,"Vdd",1.3; FTST?
    SHIT? "it's quoted",,(((FOO, BAR),(@@)),7,1.3),#9000000004asdf

We'd like to get these commands into structure like:
"""

qc"""
type FirmwareCommand
    command::String
    isquery::Bool
    parameters
end
"""

md"""
We build on the regular expressions that we know and love. First, let's just
try to parse:

    FTST?

Into:
"""

qc"""
FirmwareCommand("FTST",true,[])
"""

md"""
This is easily matched with a regex:
"""

qcr"""
match(r"(\w{4})(\??)", "FTST?")
"""

md"""
And almost as easily with a grammar:
"""

qc"""
g = grammar\"""
    statement = command isquery
    command = ~"\w{4}"
    isquery = "?"?
    \"""
"""

qcr"""
tree = parse(g, "FTST?")
"""

md"""
It's a parse tree. The goal is to fold it up.
"""

qc"""
type FwVis <: NodeVisitor end
visit(::FwVis, n::LeafNode) = nodetext(n)
visit(::FwVis, n::ParentNode, visited_children...) = visited_children
"""

# TODO: qcr fails with multiple commands. Make it work like qc
# TODO: qcr doesn't indent output printed to STDOUT. Need to capture and indent or maybe just use ``` for now

qcr"""
debug_govisit(FwVis(), tree)
"""

visit(::FwVis, n::ParentNode{:isquery}, questionmark) = questionmark == "?"
visit(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool) = FirmwareCommand(command, isquery, [])


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
debug_govisit(FwVis(), tree)

tryit("FTST?")

# With out the '?' isquery is a leaf
visit(::FwVis, n::LeafNode{:isquery}) = false

tryit("FTST")
#@show visit(FwVis(), txt, tree; debug=true)

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

visit(::FwVis, n::ParentNode{:statements}, statements...) = [statement_term[1] for statement_term in statements]
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

visit(::FwVis, n::LeafNode{:somespace}) = nothing
@show fwtest("statements", g,  "ASDF;ASDF 1;ASDF 1,2;ASDF one;ASDF one,two;")

visit(::FwVis, n::LeafNode{:comma}) = nothing
visit(::FwVis, n::ParentNode{:even_more_params}, boxes) = [box for box in boxes]
visit(::FwVis, n::ParentNode{:more_params}, param, even_more_params) = vcat([param], even_more_params)
visit(::FwVis, n::ParentNode{:one_param}, param) = [param]
visit(::FwVis, n::LeafNode{:no_params}) = []
visit(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool, params) = FirmwareCommand(command, isquery, params[1])

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
visit(::FwVis, n::ParentNode{:topparams}, params...) = params
visit(::FwVis, n::ParentNode{:param}, param...) = param[1]
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

type FirmwareVisitor <: NodeVisitor end

function DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:statement}, visited_children)
    _, command, isquery, parameters, _ = visited_children
    FirmwareCommand(command, isquery, parameters)
end

DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:isquery}, visited_children) = visited_children[1] == "?"
DEFUNCT_visit(::FirmwareVisitor, n::LeafNode{:isquery}) = false

function DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:paramlist}, visited_children)
    _, param, _, more_params = visited_children
    @show param more_params
    vcat([param], more_params)
end

function DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:more_params}, visited_children)
    [vc[3] for vc in visited_children]
end

DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:param}, visited_children) = visited_children[1]
DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:nonemptyparam}, visited_children) = visited_children[1]
DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:statements}, visited_children) = visited_children[1]
DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:params}, visited_children) = visited_children[1]
DEFUNCT_visit(::FirmwareVisitor, n::LeafNode{:noparams}) = []
DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:oneparam}, visited_children) = [visited_children[2]]
DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:litparam}, visited_children) = visited_children[1]
DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:quotedparam}, visited_children) = visited_children[2]
DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:parenthesizedparam}, visited_children) = visited_children[2]
DEFUNCT_visit(::FirmwareVisitor, n::ParentNode{:emptyparam}, visited_children) = []

DEFUNCT_visit(::FirmwareVisitor, n::ParentNode, visited_children) = visited_children
DEFUNCT_visit(::FirmwareVisitor, n::LeafNode, visited_children) = nodetext(n)
DEFUNCT_visit(::FirmwareVisitor, n::LeafNode) = nodetext(n)

firmware_statements = DEFUNCT_visit(FirmwareVisitor(), sample_text, tree)
# TODO: make keyword arguments work right
# firmware_statements = DEFUNCT_visit(FirmwareVisitor(), sample_text, tree; debug=true)

@show firmware_statements
end
