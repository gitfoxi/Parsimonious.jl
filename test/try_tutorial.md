
Parsimonious.jl
===============

A port of [Parsimonious](https://github.com/erikrose/parsimonious) by the incomperable [Erik Rose](https://github.com/erikrose).

This is my Learn Julia project and it's been pretty interesting.

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

```jl
julia> match(r"(\w{4})(\??)", "FTST?")
RegexMatch("FTST?", 1="FTST", 2="?")```

And almost as easily with a grammar:

```jl

g = grammar"""
    statement = command isquery
    command = ~"\w{4}"
    isquery = "?"?
    """
```
```jl
julia> tree = parse(g, "FTST?")
1 ParentNode{:statement}                                                'FTST?'
  .  1 LeafNode{:command}                                                'FTST'
  .  5 ParentNode{:isquery}                                                 '?'
  .    .  5 LeafNode{:}                                                     '?'```

It's a parse tree. The goal is to fold it up.

```jl

type FwVis <: NodeVisitor end
```
```jl
julia> debug_govisit(FwVis(), tree)
("FTST",("?",))
---------------------------------------------------------------------------------------------------------------
 . visit(::NodeVisitor,n::LeafNode{T})                       | command   | "FTST" -> "FTST"                  | 
 .  . visit(::NodeVisitor,n::LeafNode{T})                    |           | "?" -> "?"                        | 
 . visit(::NodeVisitor,n::ParentNode{T},visited_children...) | isquery   | "?" -> ("?",)                     | 
visit(::NodeVisitor,n::ParentNode{T},visited_children...)    | statement | "FTST", ("?",) -> ("FTST",("?",)) | 
---------------------------------------------------------------------------------------------------------------```

We haven't defined any visit methods but we get two for free. These fold the
tree up into `("FTST",("?",))` which is almost useful, but not really what
we want. Let's redefine those free rules so we can see what they do.

```jl

visit(::FwVis, n::LeafNode) = nodetext(n)
visit(::FwVis, n::ParentNode, visited_children...) = visited_children
```
```jl
julia> debug_govisit(FwVis(), tree)
("FTST",("?",))
---------------------------------------------------------------------------------------------------------
 . visit(::FwVis,n::LeafNode{T})                       | command   | "FTST" -> "FTST"                  | 
 .  . visit(::FwVis,n::LeafNode{T})                    |           | "?" -> "?"                        | 
 . visit(::FwVis,n::ParentNode{T},visited_children...) | isquery   | "?" -> ("?",)                     | 
visit(::FwVis,n::ParentNode{T},visited_children...)    | statement | "FTST", ("?",) -> ("FTST",("?",)) | 
---------------------------------------------------------------------------------------------------------```

Works the same. The `LeafNode` rule just returns whatever text the leaf rule
matched. The `ParentNode` rule takes the text from one or more `LeafNode`s and
returns a list. The list is in order. That's important.

Now, let's put the visitor to work doing our bidding.

```jl

visit(::FwVis, n::ParentNode{:isquery}, questionmark) = questionmark == "?"
visit(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool) = FirmwareCommand(command, isquery, [])
```
```jl
julia> debug_govisit(FwVis(), tree)
FirmwareCommand("FTST",true,[])
---------------------------------------------------------------------------------------------------------------------------------------
 . visit(::FwVis,n::LeafNode{T})                                       | command   | "FTST" -> "FTST"                                | 
 .  . visit(::FwVis,n::LeafNode{T})                                    |           | "?" -> "?"                                      | 
 . visit(::FwVis,n::ParentNode{:isquery},questionmark)                 | isquery   | "?" -> true                                     | 
visit(::FwVis,n::ParentNode{:statement},command::String,isquery::Bool) | statement | "FTST", true -> FirmwareCommand("FTST",true,[]) | 
---------------------------------------------------------------------------------------------------------------------------------------```

Cool. Now we're getting back `FirmwareCommand("FTST",true,[])` which is what we
wanted. Of course, you won't want to see the debug info when you're done:

```jl
julia> govisit(FwVis(), tree)
FirmwareCommand("FTST",true,[])```

Ya dig? Okay, now we add features to the language. The grammar should already
support:

    FTST

Does it?

```jl
julia> tree = parse(g, "FTST")
1 ParentNode{:statement}                                                 'FTST'
  .  1 LeafNode{:command}                                                'FTST'
  .  5 LeafNode{:isquery}                                                    ''```

It does. Let's visit:

```jl
julia> debug_govisit(FwVis(), tree)
("FTST","")
----------------------------------------------------------------------------------------------
 . visit(::FwVis,n::LeafNode{T})                    | command   | "FTST" -> "FTST"          | 
 . visit(::FwVis,n::LeafNode{T})                    | isquery   | "" -> ""                  | 
visit(::FwVis,n::ParentNode{T},visited_children...) | statement | "FTST", "" -> ("FTST","") | 
----------------------------------------------------------------------------------------------```

Ohs nos! What happened? Looking carefully we see that `:isquery` is no-longer a
ParentNode but a LeafNode because nothing matched below `isquery` (but
`isquery` matched the nothing). So we add a `visit`:

```jl

visit(::FwVis, n::LeafNode{:isquery}) = false
```

No questionmark means no query. Let's try again.

```jl
julia> govisit(FwVis(), tree)
FirmwareCommand("FTST",false,[])```

Great. How to recognize multiple statements? Noticing in the examples that
statements can be separated by ';' or '\n', let's add a rule to recognize
breaks between statements:

    termination = ~'[;\\n]'

And another one to recognize multiple statements:

    statements = statement+

So now the grammar looks like:

```jl

    g = grammar"""
        statements = (statement termination)+
        statement = command isquery
        command = ~"\w{4}"
        isquery = "?"?
        termination = ~'[;\n]'
        """
```

It's important to notice that the grammar now matches `statements` instead of
`statement` so our previous tests are now broken. Usually `parse` matches the
first statement in the list, but for testing purposes you can parse against any
of the statements. Let's check the old functionality:

```jl
julia> govisit(FwVis(), parse(g.exprs["statement"], "FTST?"))
FirmwareCommand("FTST",true,[])```

Good. And the other one:

```jl
julia> govisit(FwVis(), parse(g.exprs["statement"], "FTST"))
FirmwareCommand("FTST",false,[])```
```jl

using Base.Test
```
```jl

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
```

So we haven't lost ground. Now let's see if we can string some statements
together:

```jl
julia> fwtest("statements", g, "FTST?;FTST?;FTST?\nFTST?\nFTST;FTST\n")
((FirmwareCommand("FTST",true,[]),";"),(FirmwareCommand("FTST",true,[]),";"),(FirmwareCommand("FTST",true,[]),"\n"),(FirmwareCommand("FTST",true,[]),"\n"),(FirmwareCommand("FTST",false,[]),";"),(FirmwareCommand("FTST",false,[]),"\n"))```

Which almost looks good, but clearly we need to `visit` `statemets` differently
to throw away the useless terminations and just return a list of statements.

```jl

visit(::FwVis, n::ParentNode{:statements}, statements...) = [statement_term[1] for statement_term in statements]
```

Try it again:

```jl
julia> fwtest("statements", g, "FTST?;FTST?;FTST?\nFTST?\nFTST;FTST\n")
{FirmwareCommand("FTST",true,[]),FirmwareCommand("FTST",true,[]),FirmwareCommand("FTST",true,[]),FirmwareCommand("FTST",true,[]),FirmwareCommand("FTST",false,[]),FirmwareCommand("FTST",false,[])}```

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

```jl

g = grammar"""
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
    """
```

I don't really want to think about it so let's just look at debug and go from
there.  
```jl

txt = "ASDF;ASDF 1;ASDF 1,2,3,4;"
tree = parse(g, txt)
```
```jl
julia> debug_govisit(FwVis(), tree)
{("ASDF",false,("",)),("ASDF",false,((" ",("1",)),)),("ASDF",false,((" ",(("1",((",","2"),(",","3"),(",","4"))),)),))}
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 .  .  . visit(::FwVis,n::LeafNode{T})                                     | command     | "ASDF" -> "ASDF"                                                           | 
 .  .  . visit(::FwVis,n::LeafNode{:isquery})                              | isquery     | "" -> false                                                                | 
 .  .  .  . visit(::FwVis,n::LeafNode{T})                                  | no_params   | "" -> ""                                                                   | 
 .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)               | params      | "" -> ("",)                                                                | 
 .  . visit(::FwVis,n::ParentNode{T},visited_children...)                  | statement   | "ASDF", false, ("",) -> ("ASDF",false,("",))                               | 
 .  . visit(::FwVis,n::LeafNode{T})                                        | termination | ";" -> ";"                                                                 | 
 . visit(::FwVis,n::ParentNode{T},visited_children...)                     |             | ("ASDF",false,("",)), ";" -> (("ASDF",false,("",)),";")                    | 
 .  .  . visit(::FwVis,n::LeafNode{T})                                     | command     | "ASDF" -> "ASDF"                                                           | 
 .  .  . visit(::FwVis,n::LeafNode{:isquery})                              | isquery     | "" -> false                                                                | 
 .  .  .  .  . visit(::FwVis,n::LeafNode{T})                               | somespace   | " " -> " "                                                                 | 
 .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                            | param       | "1" -> "1"                                                                 | 
 .  .  .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)         |             | "1" -> ("1",)                                                              | 
 .  .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)            | some_params | " ", ("1",) -> (" ",("1",))                                                | 
 .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)               | params      | (" ",("1",)) -> ((" ",("1",)),)                                            | 
 .  . visit(::FwVis,n::ParentNode{T},visited_children...)                  | statement   | "ASDF", false, ((" ",("1",)),) -> ("ASDF",false,((" ",("1",)),))           | 
 .  . visit(::FwVis,n::LeafNode{T})                                        | termination | ";" -> ";"                                                                 | 
 . visit(::FwVis,n::ParentNode{T},visited_children...)                     |             | ("ASDF",false,((" ",("1",)),)), ";" -> (("ASDF",false,((" ",("1",)),)),";" | 
                                                                           |             | )                                                                          | 
 .  .  . visit(::FwVis,n::LeafNode{T})                                     | command     | "ASDF" -> "ASDF"                                                           | 
 .  .  . visit(::FwVis,n::LeafNode{:isquery})                              | isquery     | "" -> false                                                                | 
 .  .  .  .  . visit(::FwVis,n::LeafNode{T})                               | somespace   | " " -> " "                                                                 | 
 .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                         | param       | "1" -> "1"                                                                 | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                   | comma       | "," -> ","                                                                 | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                   | param       | "2" -> "2"                                                                 | 
 .  .  .  .  .  .  .  . visit(::FwVis,n::ParentNode{T},visited_children... |             | ",", "2" -> (",","2")                                                      | 
)                                                                          |             |                                                                            | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                   | comma       | "," -> ","                                                                 | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                   | param       | "3" -> "3"                                                                 | 
 .  .  .  .  .  .  .  . visit(::FwVis,n::ParentNode{T},visited_children... |             | ",", "3" -> (",","3")                                                      | 
)                                                                          |             |                                                                            | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                   | comma       | "," -> ","                                                                 | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                   | param       | "4" -> "4"                                                                 | 
 .  .  .  .  .  .  .  . visit(::FwVis,n::ParentNode{T},visited_children... |             | ",", "4" -> (",","4")                                                      | 
)                                                                          |             |                                                                            | 
 .  .  .  .  .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)   |             | (",","2"), (",","3"), (",","4") -> ((",","2"),(",","3"),(",","4"))         | 
 .  .  .  .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)      | more_params | "1", ((",","2"),(",","3"),(",","4")) -> ("1",((",","2"),(",","3"),(",","4" | 
                                                                           |             | )))                                                                        | 
 .  .  .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)         |             | ("1",((",","2"),(",","3"),(",","4"))) -> (("1",((",","2"),(",","3"),(","," | 
                                                                           |             | 4"))),)                                                                    | 
 .  .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)            | some_params | " ", (("1",((",","2"),(",","3"),(",","4"))),) -> (" ",(("1",((",","2"),(", | 
                                                                           |             | ","3"),(",","4"))),))                                                      | 
 .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)               | params      | (" ",(("1",((",","2"),(",","3"),(",","4"))),)) -> ((" ",(("1",((",","2"),( | 
                                                                           |             | ",","3"),(",","4"))),)),)                                                  | 
 .  . visit(::FwVis,n::ParentNode{T},visited_children...)                  | statement   | "ASDF", false, ((" ",(("1",((",","2"),(",","3"),(",","4"))),)),) -> ("ASDF | 
                                                                           |             | ",false,((" ",(("1",((",","2"),(",","3"),(",","4"))),)),))                 | 
 .  . visit(::FwVis,n::LeafNode{T})                                        | termination | ";" -> ";"                                                                 | 
 . visit(::FwVis,n::ParentNode{T},visited_children...)                     |             | ("ASDF",false,((" ",(("1",((",","2"),(",","3"),(",","4"))),)),)), ";" -> ( | 
                                                                           |             | ("ASDF",false,((" ",(("1",((",","2"),(",","3"),(",","4"))),)),)),";")      | 
visit(::FwVis,n::ParentNode{:statements},statements...)                    | statements  | (("ASDF",false,("",)),";"), (("ASDF",false,((" ",("1",)),)),";"), (("ASDF" | 
                                                                           |             | ,false,((" ",(("1",((",","2"),(",","3"),(",","4"))),)),)),";") -> {("ASDF" | 
                                                                           |             | ,false,("",)),("ASDF",false,((" ",("1",)),)),("ASDF",false,((" ",(("1",((" | 
                                                                           |             | ,","2"),(",","3"),(",","4"))),)),))}                                       | 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------```

Wow. The debug output barely fits on my 13-inch laptop screen. I wish I could
come up with something more compact and just as useful, but this is addressing
several common problems:

* Parse not matching exactly the way I expected
* Wrong `visit` method handles a node

If you have a better idea, let me know.

`visit` time:

```jl

visit(::FwVis, n::LeafNode{:no_params}) = nothing
visit(::FwVis, n::LeafNode{:comma}) = nothing
visit(::FwVis, n::LeafNode{:somespace}) = nothing
visit(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool, params) = FirmwareCommand(command, isquery, params)
visit(::FwVis, n::ParentNode{:more_params}, param1::String, param2::String) = (param1, param2)
visit(::FwVis, n::ParentNode{:more_params}, param::String, params::Tuple) = tuple(param, params...)
visit(::FwVis, n::ParentNode, one_child) = one_child # don't rebox one child
```

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

```jl
julia> debug_govisit(FwVis(), tree)
{FirmwareCommand("ASDF",false,()),FirmwareCommand("ASDF",false,"1"),FirmwareCommand("ASDF",false,("1","2","3","4"))}
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 .  .  . visit(::FwVis,n::LeafNode{T})                                     | command     | "ASDF" -> "ASDF"                                                           | 
 .  .  . visit(::FwVis,n::LeafNode{:isquery})                              | isquery     | "" -> false                                                                | 
 .  .  .  . visit(::FwVis,n::LeafNode{:no_params})                         | no_params   | "" -> nothing                                                              | 
 .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)               | params      |  -> ()                                                                     | 
 .  . visit(::FwVis,n::ParentNode{:statement},command::String,isquery::Boo | statement   | "ASDF", false, () -> FirmwareCommand("ASDF",false,())                      | 
l,params)                                                                  |             |                                                                            | 
 .  . visit(::FwVis,n::LeafNode{T})                                        | termination | ";" -> ";"                                                                 | 
 . visit(::FwVis,n::ParentNode{T},visited_children...)                     |             | FirmwareCommand("ASDF",false,()), ";" -> (FirmwareCommand("ASDF",false,()) | 
                                                                           |             | ,";")                                                                      | 
 .  .  . visit(::FwVis,n::LeafNode{T})                                     | command     | "ASDF" -> "ASDF"                                                           | 
 .  .  . visit(::FwVis,n::LeafNode{:isquery})                              | isquery     | "" -> false                                                                | 
 .  .  .  .  . visit(::FwVis,n::LeafNode{:somespace})                      | somespace   | " " -> nothing                                                             | 
 .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                            | param       | "1" -> "1"                                                                 | 
 .  .  .  .  . visit(::FwVis,n::ParentNode{T},one_child)                   |             | "1" -> "1"                                                                 | 
 .  .  .  . visit(::FwVis,n::ParentNode{T},one_child)                      | some_params | "1" -> "1"                                                                 | 
 .  .  . visit(::FwVis,n::ParentNode{T},one_child)                         | params      | "1" -> "1"                                                                 | 
 .  . visit(::FwVis,n::ParentNode{:statement},command::String,isquery::Boo | statement   | "ASDF", false, "1" -> FirmwareCommand("ASDF",false,"1")                    | 
l,params)                                                                  |             |                                                                            | 
 .  . visit(::FwVis,n::LeafNode{T})                                        | termination | ";" -> ";"                                                                 | 
 . visit(::FwVis,n::ParentNode{T},visited_children...)                     |             | FirmwareCommand("ASDF",false,"1"), ";" -> (FirmwareCommand("ASDF",false,"1 | 
                                                                           |             | "),";")                                                                    | 
 .  .  . visit(::FwVis,n::LeafNode{T})                                     | command     | "ASDF" -> "ASDF"                                                           | 
 .  .  . visit(::FwVis,n::LeafNode{:isquery})                              | isquery     | "" -> false                                                                | 
 .  .  .  .  . visit(::FwVis,n::LeafNode{:somespace})                      | somespace   | " " -> nothing                                                             | 
 .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                         | param       | "1" -> "1"                                                                 | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{:comma})              | comma       | "," -> nothing                                                             | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                   | param       | "2" -> "2"                                                                 | 
 .  .  .  .  .  .  .  . visit(::FwVis,n::ParentNode{T},one_child)          |             | "2" -> "2"                                                                 | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{:comma})              | comma       | "," -> nothing                                                             | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                   | param       | "3" -> "3"                                                                 | 
 .  .  .  .  .  .  .  . visit(::FwVis,n::ParentNode{T},one_child)          |             | "3" -> "3"                                                                 | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{:comma})              | comma       | "," -> nothing                                                             | 
 .  .  .  .  .  .  .  .  . visit(::FwVis,n::LeafNode{T})                   | param       | "4" -> "4"                                                                 | 
 .  .  .  .  .  .  .  . visit(::FwVis,n::ParentNode{T},one_child)          |             | "4" -> "4"                                                                 | 
 .  .  .  .  .  .  . visit(::FwVis,n::ParentNode{T},visited_children...)   |             | "2", "3", "4" -> ("2","3","4")                                             | 
 .  .  .  .  .  . visit(::FwVis,n::ParentNode{:more_params},param::String, | more_params | "1", ("2","3","4") -> ("1","2","3","4")                                    | 
params::(Any...,))                                                         |             |                                                                            | 
 .  .  .  .  . visit(::FwVis,n::ParentNode{T},one_child)                   |             | ("1","2","3","4") -> ("1","2","3","4")                                     | 
 .  .  .  . visit(::FwVis,n::ParentNode{T},one_child)                      | some_params | ("1","2","3","4") -> ("1","2","3","4")                                     | 
 .  .  . visit(::FwVis,n::ParentNode{T},one_child)                         | params      | ("1","2","3","4") -> ("1","2","3","4")                                     | 
 .  . visit(::FwVis,n::ParentNode{:statement},command::String,isquery::Boo | statement   | "ASDF", false, ("1","2","3","4") -> FirmwareCommand("ASDF",false,("1","2", | 
l,params)                                                                  |             | "3","4"))                                                                  | 
 .  . visit(::FwVis,n::LeafNode{T})                                        | termination | ";" -> ";"                                                                 | 
 . visit(::FwVis,n::ParentNode{T},visited_children...)                     |             | FirmwareCommand("ASDF",false,("1","2","3","4")), ";" -> (FirmwareCommand(" | 
                                                                           |             | ASDF",false,("1","2","3","4")),";")                                        | 
visit(::FwVis,n::ParentNode{:statements},statements...)                    | statements  | (FirmwareCommand("ASDF",false,()),";"), (FirmwareCommand("ASDF",false,"1") | 
                                                                           |             | ,";"), (FirmwareCommand("ASDF",false,("1","2","3","4")),";") -> {FirmwareC | 
                                                                           |             | ommand("ASDF",false,()),FirmwareCommand("ASDF",false,"1"),FirmwareCommand( | 
                                                                           |             | "ASDF",false,("1","2","3","4"))}                                           | 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------```
