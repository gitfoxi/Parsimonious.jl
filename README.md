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

We'd like to get these commands into structure like:

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

    g = grammar"""
        statement = command isquery
        command = ~"\w{4}"
        isquery = "?"?
        """

    julia> @show tree = parse(g, "FTST?")
    tree = parse(g,"FTST?") => ParentNode{:statement}(OneOrMoreMatch(1,5),(ChildlessNode{:command}(OneOrMoreMatch(1,4)),ParentNode{:isquery}(OneOrMoreMatch(5,5),(ChildlessNode{:}(OneOrMoreMatch(5,5)),))))

Huh. Let's look at this another way:

    julia> println(tree)
    1 ParentNode{:statement}                                                'FTST?'
      .  1 LeafNode{:command}                                                'FTST'
      .  5 ParentNode{:isquery}                                                 '?'
      .    .  5 LeafNode{:}                                                     '?'

That's better. But still not the structure we want. Let's write some visitors
to fix it up.

    type FwVis <: NodeVisitor end
    visit2(::FwVis, n::ParentNode{:isquery}, questionmark) = questionmark == "?"
    visit2(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool) = FirmwareCommand(command, isquery, [])
    visit2(::FwVis, n::LeafNode) = nodetext(n)
    visit2(::FwVis, n::ParentNode, visited_children...) = visited_children

And sick them on the tree:

    julia> govisit(FwVis(), tree)
    FirmwareCommand("FTST", true, [])

Nice. But what exactly did the visitor do? Let's visit in debug mode to see:

    julia> govisit(FwVis(), tree; debug=true)

This traces the visit calls which can be really usefull debugging.

    visit2(::FwVis,n::LeafNode{T}) at /Users/m/s/m/current/Parsimonious.jl/test_firmware.jl:62
    returns => "FTST"
    visit2(::FwVis,n::LeafNode{T}) at /Users/m/s/m/current/Parsimonious.jl/test_firmware.jl:62
    returns => "?"
    visit2(::FwVis,n::ParentNode{:isquery},questionmark) at /Users/m/s/m/current/Parsimonious.jl/test_firmware.jl:60
    returns => true
    visit2(::FwVis,n::ParentNode{:statement},command::String,isquery::Bool) at /Users/m/s/m/current/Parsimonious.jl/test_firmware.jl:61
    returns => FirmwareCommand("FTST",true,[])

Let's take it step-by-step. `govisit` takes a visitor and a parse tree. It
starts at a LeafNode and calls a visitor written for that specific node name.
If the node doesn't have a name or there hasn't been a specific `visit` written
for it then it falls back to a generic one like in this case:

      .    .  5 LeafNode{:}                                                     '?'

Gets visited by the generic:

    visit2(::FwVis, n::LeafNode) = nodetext(n)

Which simply:

    returns => "?"

Returns the text.

After all the leaves under a parent have been visited, the parent is visited,
only this time the `visit` has additional arguments, one for whatever each leaf
returned in order. So when we get to:

      .  5 ParentNode{:isquery}                                                 '?'

The Leaf under it has returned "?" so it calls the function:

    visit2(::FwVis, n::ParentNode{:isquery}, questionmark) = questionmark == "?"

Like this:

    visit2(FwVis,n,"?")

Which:

    returns => true

Ya dig? Okay, now we add features to the language. The grammar should already
support:

    FTST

Does it?

    tree = parse(g, "FTST")
    println(tree)

    1 ParentNode{:statement}                                                 'FTST'
      .  1 LeafNode{:command}                                                'FTST'
      .  5 LeafNode{:isquery}                                                    ''

It does. Let's visit:

    julia> govisit(FwVis(), tree; debug=true)
    visit2(::FwVis,n::LeafNode{T}) at /Users/m/s/m/current/Parsimonious.jl/test_firmware.jl:62
    returns => "FTST"
    visit2(::FwVis,n::LeafNode{T}) at /Users/m/s/m/current/Parsimonious.jl/test_firmware.jl:62
    returns => ""
    visit2(::FwVis,n::ParentNode{T},visited_children...) at /Users/m/s/m/current/Parsimonious.jl/test_firmware.jl:63
    returns => ("FTST","")

Ohs nos! What happened? Looking carefully we see that `:isquery` is no-longer a
ParentNode but a LeafNode because nothing matched below `isquery` (but
`isquery` matched the nothing). So we add `visit`:

    visit2(::FwVis, n::LeafNode{:isquery}) = false

No questionmark means no query. Let's try again.

    julia> govisit(FwVis(), tree)
    FirmwareCommand("FTST",false,[])

Great. How to recognize multiple statements? Recognizing from the examples that
they can be separated by ';' or '\n', let's add a rule to recognize breaks
between statements:

    termination = ~'[;\\n]'

And another one to recognize multiple statements:

    statements = statement+

So now the grammar looks like:

    g = grammar"""
        statements = statement+
        statement = command isquery
        command = ~"\w{4}"
        isquery = "?"?
        termination = ~'[;\\n]'
        """

It's important to notice that the grammar now matches `statements` instead of
`statement` so our previous tests are now broken. Usually `parse` matches the
first statement in the list, but for testing purposes you can parse against any
of the statements. Let's check the old functionality:

    julia> tree = parse(g.exprs["statement"], "FTST?")
    julia> govisit(FwVis(), tree)
    FirmwareCommand("FTST",true,[])

Good. And the other one:

    julia> tree = parse(g.exprs["statement"], "FTST")
    julia> govisit(FwVis(), tree)
    FirmwareCommand("FTST",false,[])

Fine. Now would be a good time to start making tests to guard against
regressions as we develop.

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

    using Base.Test
    @test fwtest("statement", g, "FTST?") == FirmwareCommand("FTST",true,[])
    @test fwtest("statement", g, "FTST") == FirmwareCommand("FTST",false,[])

So we haven't lost ground. Now let's see if we can string some statements
together:

    fwtest("statements", g, "FTST?;FTST?;FTST?\nFTST?\nFTST;FTST\n")

Which gives:

    ((FirmwareCommand("FTST",true,[]),";"),(FirmwareCommand("FTST",true,[]),";"),(FirmwareCommand("FTST",true,[]),"\n"),(FirmwareCommand("FTST",true,[]),"\n"),(FirmwareCommand("FTST",false,[]),";"),(FirmwareCommand("FTST",false,[]),"\n"))

Which almost looks good, but clearly we need to `visit` `statemets` differently
to throw away the useless terminations and just return a list of statements.

    visit2(::FwVis, n::ParentNode{:statements}, statements...) = [statement_term[1] for statement_term in statements]

Try it again:

    {FirmwareCommand("FTST",true,[]),FirmwareCommand("FTST",true,[]),FirmwareCommand("FTST",true,[]),FirmwareCommand("FTST",true,[]),FirmwareCommand("FTST",false,[]),FirmwareCommand("FTST",false,[])}

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

    statement = command isquery params
    params = more_params / one_param / no_params
    more_params = somespace param (',' param)+
    one_param = somespace param
    param = ~'[^ \t,;\n]*'
    no_params = ''
    somespace = ~'[ \t]+'

Let's look at the tree and then try to write some `visit` functions:

    julia> tree = parse(g, "ASDF;ASDF 1;ASDF 1,2;")
    julia> println(tree)

It's looking a little more sophisticated:

    1 ParentNode{:statements}                               'ASDF;ASDF 1;ASDF 1,2;'
      .  1 ParentNode{:}                                                    'ASDF;'
      .    .  1 ParentNode{:statement}                                       'ASDF'
      .    .    .  1 LeafNode{:command}                                      'ASDF'
      .    .    .  5 LeafNode{:isquery}                                          ''
      .    .    .  5 ParentNode{:params}                                         ''
      .    .    .    .  5 LeafNode{:no_params}                                   ''
      .    .  5 LeafNode{:termination}                                          ';'
      .  6 ParentNode{:}                                                  'ASDF 1;'
      .    .  6 ParentNode{:statement}                                     'ASDF 1'
      .    .    .  6 LeafNode{:command}                                      'ASDF'
      .    .    .  10 LeafNode{:isquery}                                         ''
      .    .    .  10 ParentNode{:params}                                      ' 1'
      .    .    .    .  10 ParentNode{:one_param}                              ' 1'
      .    .    .    .    .  10 LeafNode{:somespace}                            ' '
      .    .    .    .    .  11 LeafNode{:param}                                '1'
      .    .  12 LeafNode{:termination}                                         ';'
      .  13 ParentNode{:}                                               'ASDF 1,2;'
      .    .  13 ParentNode{:statement}                                  'ASDF 1,2'
      .    .    .  13 LeafNode{:command}                                     'ASDF'
      .    .    .  17 LeafNode{:isquery}                                         ''
      .    .    .  17 ParentNode{:params}                                    ' 1,2'
      .    .    .    .  17 ParentNode{:more_params}                          ' 1,2'
      .    .    .    .    .  17 LeafNode{:somespace}                            ' '
      .    .    .    .    .  18 LeafNode{:param}                                '1'
      .    .    .    .    .  19 ParentNode{:even_more_params}                  ',2'
      .    .    .    .    .    .  19 ParentNode{:}                             ',2'
      .    .    .    .    .    .    .  19 LeafNode{:}                           ','
      .    .    .    .    .    .    .  20 LeafNode{:param}                      '2'
      .    .  21 LeafNode{:termination}                                         ';'

This was a lot of rules to add at once and admittedly I had to do some trial
and error with some things I hadn't thought through all the way. If it doesn't
work, read the error messages. They try very hard to help you.

Okay, we'll need some `visit` functions to roll up `even_more_params`,
`more_params`, `one_param` and we'll need to fix `statement` to account for
it's new opperand `params`.

One feature I forgot to mention -- because I just added it while typing the
tutorial -- is that if `visit` returns `nothing` then it will get wiped from
thing passed to it's parent's visit like it never happened. We can use this to
get rid of useless `somespace`.

    visit2(::FwVis, n::LeafNode{:somespace}) = nothing

Testing this it looks good though a little messed up because we haven't dealth
with `visit`s to the other things:

    julia> fwtest("statements", g,  "ASDF;ASDF 1;ASDF 1,2;")
    fwtest("statements",g,"ASDF;ASDF 1;ASDF 1,2;") => {("ASDF",false,("",)),("ASDF",false,(("1",),)),("ASDF",false,(("1",((",","2"),)),))}

So, let's do that:

    visit2(::FwVis, n::LeafNode{:comma}) = nothing
    visit2(::FwVis, n::ParentNode{:even_more_params}, boxes) = [box for box in boxes]
    visit2(::FwVis, n::ParentNode{:more_params}, param, even_more_params) = vcat([param], even_more_params)
    visit2(::FwVis, n::ParentNode{:one_param}, param) = [param]
    visit2(::FwVis, n::LeafNode{:no_params}) = []
    visit2(::FwVis, n::ParentNode{:statement}, command::String, isquery::Bool, params) = FirmwareCommand(command, isquery, params[1])

Obviously I didn't crap this out fully formed. I wrote and tested each line
carefully refering back to the parse tree and the debug output.

Next we realize that there's different kinds of parameters. We're just
supporting bare words, but you can also have quoted strings and nested,
parenthesized lists. Updating the param rule to look like:

    param = bare / quoted / list
    bare = ~'[^ \t,;\n")(]*'
    dq = '"'
    quoted = dq ~'[^"]' dq
    open_paren = '('
    close_paren = ')'
    list = open_paren params close_paren

The whole grammar is getting quite long, but is still pretty easy to read and
understand.

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
        no_params = ''
        somespace = ~'[ \t]+'
        param = bare / quoted / list
        bare = ~'[^ \t,;\n")(]*'
        dq = '"'
        quoted = dq ~'[^"]' dq
        open_paren = '('
        close_paren = ')'
        list = open_paren params close_paren
        """

As usual, we try it against a test string:

