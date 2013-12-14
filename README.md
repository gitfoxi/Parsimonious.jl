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

    using Parsimonious

The key is to remain calm and organized at all times. Otherwise parser
development will get away from you. Our Firmware Command DSL was designed by
some fuckwit to communicate with an HP 93k tester. It looks like this:

    FTST?
    FTST P
    SREC? (@)
    SREC ACT,(@)
    SVLR TIM,PRM,,"Vdd",1.3; FTST?
    SHIT? "it's quoted",,(((FOO, BAR),(@@)),7,1.3),#9000000004asdf

We'd like to get tehse commands into structure like:

    type FirmwareCommand
        command::ASCIIString
        isquery::Bool
        parameters
    end

We build on the regular expressions that we know and love. First, let's just
try to parse:

    FTST?

Into:

    FirmwareCommand("FTST", true, [])

This is easily matched with a regex:

    julia> match(r"(\w{4})(\??)", "FTST?")
    RegexMatch("FTST?", 1="FTST", 2="?")

And almost as easily with a grammar:

    julia> g = grammar"""
        statement = command isquery
        command = ~"\w{4}"
        isquery = "?"?
        """

    julia> @show tree = parse(g, "FTST?")
    tree = parse(g,"FTST?") => ParentNode{:statement}(OneOrMoreMatch(1,5),(ChildlessNode{:command}(OneOrMoreMatch(1,4)),ParentNode{:isquery}(OneOrMoreMatch(5,5),(ChildlessNode{:}(OneOrMoreMatch(5,5)),))))

Huh. Let's look at this another way:

    julia> pprettily(tree)
    <statement matching 'FTST?'>
    | <command matching 'FTST'>
    | <isquery matching '?'>
    | | < matching '?'>

That's better. But still not the structure we want. Let's write some visitors
to fix it up.

    julia> type FwVis <: NodeVisitor end
    julia> visit(::FwVis, n::LeafNode) = ASCIIString(n)
    julia> visit(::FwVis, txt, n::ParentNode) = n
    julia> visit(::FwVis, n::ParentNode{:isquery}, questionmark) = questionmark == "?"
    julia> visit(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool) = FirmwareCommand(command, isquery, [])

And sick them on the tree:

    julia> visit(FwVis(), tree)
    FirmwareCommand("FTST", true, [])

Nice. But what exactly did the visitor do? Let's visit in debug mode to see:

    julia> visit(FwVis(), tree; debug=true)
