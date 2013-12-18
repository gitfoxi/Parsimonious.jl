
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
#ioout=""
        redirect_stdout(oldstdout)
        print("```jl\n",
            replace(strip($s), r"^"m, "julia> "), "\n",
            rstrip(repr(value) * "\n" * ioout))
        println("```")
    end
end
#ERROR: readcb: bad file descriptor (EBADF)
# in wait_readnb at stream.jl:282
# in readavailable at stream.jl:658
# in reload_path at loading.jl:140
# in reload at loading.jl:73
#while loading /Users/m/s/m/current/Parsimonious.jl/test/test_firmware.jl, in expression starting on line 22

# TODO: qcd(command, test_return, test_stdout) -- doctest the command too.

md"""
Parsimonious.jl
===============

A port of [Parsimonious](https://github.com/erikrose/parsimonious) by the incomperable [Erik Rose](https://github.com/erikrose).

This is my Learn Julia project and it's been pretty interesting.
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
"""

# TODO: qcr fails with multiple commands. Make it work like qc
# TODO: qcr doesn't indent output printed to STDOUT. Need to capture and indent or maybe just use ``` for now

qcr"""
debug_govisit(FwVis(), tree)
"""

md"""
We haven't defined any visit methods but we get two for free. These fold the
tree up into `("FTST",("?",))` which is almost useful, but not really what
we want. Let's redefine those free rules so we can see what they do.
"""

qc"""
visit(::FwVis, n::LeafNode) = nodetext(n)
visit(::FwVis, n::ParentNode, visited_children...) = visited_children
"""

qcr"""
debug_govisit(FwVis(), tree)
"""

md"""
Works the same. The `LeafNode` rule just returns whatever text the leaf rule
matched. The `ParentNode` rule takes the text from one or more `LeafNode`s and
returns a list. The list is in order. That's important.

Now, let's put the visitor to work doing our bidding.
"""

qc"""
visit(::FwVis, n::ParentNode{:isquery}, questionmark) = questionmark == "?"
visit(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool) = FirmwareCommand(command, isquery, [])
"""

qcr"""
debug_govisit(FwVis(), tree)
"""

md"""
Cool. Now we're getting back `FirmwareCommand("FTST",true,[])` which is what we
wanted. Of course, you won't want to see the debug info when you're done:
"""


qcr"""
govisit(FwVis(), tree)
"""

md"""
Ya dig? Okay, now we add features to the language. The grammar should already
support:

    FTST

Does it?
"""

qcr"""
tree = parse(g, "FTST")
"""

md"""
It does. Let's visit:
"""

qcr"""
debug_govisit(FwVis(), tree)
"""

md"""
Ohs nos! What happened? Looking carefully we see that `:isquery` is no-longer a
ParentNode but a LeafNode because nothing matched below `isquery` (but
`isquery` matched the nothing). So we add a `visit`:
"""

qc"""
visit(::FwVis, n::LeafNode{:isquery}) = false
"""

md"""
No questionmark means no query. Let's try again.
"""

# TODO: doctest this one
qcr"""
govisit(FwVis(), tree)
"""

md"""
Great. How to recognize multiple statements? Noticing in the examples that
statements can be separated by ';' or '\n', let's add a rule to recognize
breaks between statements:

    termination = ~'[;\\n]'

And another one to recognize multiple statements:

    statements = statement+

So now the grammar looks like:
"""

qc"""
    g = grammar\"""
        statements = (statement termination)+
        statement = command isquery
        command = ~"\w{4}"
        isquery = "?"?
        termination = ~'[;\n]'
        \"""
"""

md"""
It's important to notice that the grammar now matches `statements` instead of
`statement` so our previous tests are now broken. Usually `parse` matches the
first statement in the list, but for testing purposes you can parse against any
of the statements. Let's check the old functionality:
"""

qcr"""
govisit(FwVis(), parse(g.exprs["statement"], "FTST?"))
"""

md"""
Good. And the other one:
"""

qcr"""
govisit(FwVis(), parse(g.exprs["statement"], "FTST"))
"""

qc"""
using Base.Test
"""
# TODO: qc above and below are pointlessly split up because apparently you
# can't say `using thing` and actually use the thing in the same quote.
qc"""
function fwtest(name, grammar, text)
    tree = parse(g.exprs[name], text)
    govisit(FwVis(), tree)
end

import Base.isequal
function isequal(a::FirmwareCommand, b::FirmwareCommand)
    a.command == b.command &&
    a.isquery == b.isquery &&
    a.parameters == b.parameters
end

@test fwtest("statement", g, "FTST?") == FirmwareCommand("FTST",true,[])
@test fwtest("statement", g, "FTST") == FirmwareCommand("FTST",false,[])
"""

md"""
So we haven't lost ground. Now let's see if we can string some statements
together:
"""

qcr"""
fwtest("statements", g, "FTST?;FTST?;FTST?\nFTST?\nFTST;FTST\n")
"""

md"""
Which almost looks good, but clearly we need to `visit` `statemets` differently
to throw away the useless terminations and just return a list of statements.
"""

qc"""
visit(::FwVis, n::ParentNode{:statements}, statements...) = [statement_term[1] for statement_term in statements]
"""

md"""
Try it again:
"""

qcr"""
fwtest("statements", g, "FTST?;FTST?;FTST?\nFTST?\nFTST;FTST\n")
"""

md"""
Nice. I wonder why sometimes list print in {} and other times in []. Oh well.

There's more kinds of statements. Each can have zero one or more parameters. To
match statements like:

    ASDF
    ASDF 1
    ASDF 1,2

We'll make three rules:

    params = more_params / one_param / no_params

This is where ordered choice really comes in handy. First it will try to match
several parameters, failing that it will try to match one and only failing that
can we get by with none. Of course if we wrote:

    params = no_params / one_param / more_params

Then we'd have a problem because no_params would always match and then we'd
never look for one_param or more_params and we'd get a parsing error. Moving
on:

Also, we have to start thinking about whitespace. If a command is followed by
parameters then there's at least one space separating them. But in the no
parameter case you don't need a space. How about:

    params = some_params / no_params
    some_params = somespace (more_params / one_param)
    somespace = ~'[ \t]+'
"""

qc"""
g = grammar\"""
    statements = (statement termination)+
    command = ~"\w{4}"
    isquery = "?"?
    termination = ~'[;\n]'

    ###### new stuff ##########################
    statement = command isquery params
    params = some_params / no_params
    some_params = somespace (more_params / param)
    more_params = param (comma param)+
    param = ~'[^ \t,;\n]*'
    no_params = ''
    comma = ','
    somespace = ~'[ \t]+'
    \"""
"""

md"""
I don't really want to think about it so let's just look at debug and go from
there.  """

qc"""
txt = "ASDF;ASDF 1;ASDF 1,2,3,4;"
tree = parse(g, txt)
"""
qcr"""
debug_govisit(FwVis(), tree)
"""

md"""
Wow. The debug output barely fits on my 13-inch laptop screen. I wish I could
come up with something more compact and just as useful, but this is addressing
several common problems:

* Parse not matching exactly the way I expected
* Wrong `visit` method handles a node

If you have a better idea, let me know.

`visit` time:
"""

qc"""
visit(::FwVis, n::LeafNode{:no_params}) = nothing
visit(::FwVis, n::LeafNode{:comma}) = nothing
visit(::FwVis, n::LeafNode{:somespace}) = nothing
visit(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool, params) = FirmwareCommand(command, isquery, params)
visit(::FwVis, n::ParentNode{:more_params}, param1::String, param2::String) = (param1, param2)
visit(::FwVis, n::ParentNode{:more_params}, param::String, params::Tuple) = tuple(param, params...)
visit(::FwVis, n::ParentNode, one_child) = one_child # don't rebox one child
"""

md"""
About now we realize, oh shit, things have gotten interesting. Julia's
multiple-dispatch feature is really working hard for us.

First, we've overloaded one of the most generic visit functions just for the case when there's only one child.

    visit(::FwVis, n::ParentNode, one_child) = one_child # don't rebox one child

Remember, the original (and still existing) generic rule for ParentNode is:

    visit(::FwVis, n::ParentNode, visited_children...) = visited_children

Which basically means that several children will get boxed up. `"a", "b", "c"
-> ("a", "b", "c")`. The problem is that there's these anonymous rules
everywhere and when they get called with one child, they put it in a box. So
after several iterations, you end up with `((("a",),),)`

Also fun is using the type to fix behavior. For example:

    visit(::FwVis, n::ParentNode{:more_params}, param1::String, param2::String) = (param1, param2)
    visit(::FwVis, n::ParentNode{:more_params}, param::String, params::Tuple) = tuple(param, params...)

We really want `more_params` to just give us a list of parameters, but since the rule is nested like so:

    more_params = param (comma param)+

`visit more_params` is sometimes being called with two individual parameters
and sometimes with a parameter and another `more_params`. Using the type system
this way we make sure that what comes out is always a flat list without
having to think about it further.

Finally, notice that several garbage tokens like `no_params`, `comma` and
`some_space` return `nothing`. When a `visit` returns `nothing` we don't even
send the `nothing` to the parent. It's just like it never happened which is
good for taking out the trash.

I'm a little nervous -- don't get me wrong -- relying so heavily on the
vagaries of Julia's dispatch system for the programs logic. But it looks like
it's working! Once you go hack you never go back.

Oh, and obviously I didn't crap this out fully formed. I wrote and tested each line
carefully refering back to the debug table.
"""

qcr"""
debug_govisit(FwVis(), tree)
"""

# A quick test to guard against regression.
@test fwtest("statements", g, txt) ==
    {FirmwareCommand("ASDF",false,()),FirmwareCommand("ASDF",false,"1"),FirmwareCommand("ASDF",false,("1","2","3","4"))}


pause()

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


md"""
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

end
