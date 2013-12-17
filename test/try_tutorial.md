
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

```jl

using Parsimonious
import Parsimonious.visit

```

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

```jl

type FirmwareCommand
    command::String
    isquery::Bool
    parameters
end

```

We build on the regular expressions that we know and love. First, let's just
try to parse:

    FTST?

Into:

```jl

FirmwareCommand("FTST",true,[])

```

This is easily matched with a regex:

    julia> match(r"(\w{4})(\??)", "FTST?")
    RegexMatch("FTST?", 1="FTST", 2="?")

And almost as easily with a grammar:

```jl

g = grammar"""
    statement = command isquery
    command = ~"\w{4}"
    isquery = "?"?
    """

```
"blerg" => "blerg"
"derp" => "derp"
    julia> tree = parse(g, "FTST?")
    1 ParentNode{:statement}                                                'FTST?'
  .  1 LeafNode{:command}                                                'FTST'
  .  5 ParentNode{:isquery}                                                 '?'
  .    .  5 LeafNode{:}                                                     '?'


Huh. Let's look at this another way:

----------------------------------------------------------------------------------------------------------------------------------------
 . visit2(::FwVis,n::LeafNode{T})                                       | command   | "FTST" -> "FTST"                                | 
 .  . visit2(::FwVis,n::LeafNode{T})                                    |           | "?" -> "?"                                      | 
 . visit2(::FwVis,n::ParentNode{:isquery},questionmark)                 | isquery   | "?" -> true                                     | 
visit2(::FwVis,n::ParentNode{:statement},command::String,isquery::Bool) | statement | "FTST", true -> FirmwareCommand("FTST",true,[]) | 
----------------------------------------------------------------------------------------------------------------------------------------

1 ParentNode{:statement}                                                'FTST?'
  .  1 LeafNode{:command}                                                'FTST'
  .  5 ParentNode{:isquery}                                                 '?'
  .    .  5 LeafNode{:}                                                     '?'


