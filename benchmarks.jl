
println("Julia is Alive")
time_julia_alive = time()

using Util
using Grammars

REPEAT = 20  # Repeat parse several times and take minimum as benchmark

# As a baseline for speed, parse some JSON.
# 
# I have no reason to believe that JSON is a particularly representative or
# revealing grammar to test with. Also, this is a naive, unoptimized,
# incorrect grammar, so don't use it as a basis for comparison with other
# parsers. It's just meant to compare across versions of Parsimonious.

father = 
"""{
        "id" : 1,
        "married" : true,
        "name" : "Larry Lopez",
        "sons" : null,
        "daughters" : [
          {
            "age" : 26,
            "name" : "Sandra"
            },
          {
            "age" : 25,
            "name" : "Margaret"
            },
          {
            "age" : 6,
            "name" : "Mary"
            }
          ]
        }
"""
more_fathers = join(repeat([father], inner=[60]), ",")

sample_json = """{"fathers" : [""" * more_fathers * "]}"

not_really_json_spec = """
    value = space (string / number / object / array / true_false_null)
            space

    object = "{" members "}"
    members = (pair ("," pair)*)?
    pair = string ":" value
    array = "[" elements "]"
    elements = (value ("," value)*)?
    true_false_null = "true" / "false" / "null"

    dq = '"'
    string = space dq chars dq space
    chars = ~'[^"]*'  # TODO implement the real thing
    number = (int frac exp) / (int exp) / (int frac) / int
    int = "-"? ((digit1to9 digits) / digit)
    frac = "." digits
    exp = e digits
    digits = digit+
    e = "e+" / "e-" / "e" / "E+" / "E-" / "E"

    digit1to9 = ~"[1-9]"
    digit = ~"[0-9]"
    space = ~'\\s*'
"""

grammar = Grammar(not_really_json_spec)

# Compile
parse(grammar, sample_json)

time_compiled = time()
elapsed_compiled = time_compiled - time_julia_alive

@printf "Importing, compiling and initializing grammar done after %.2f s\n\n" elapsed_compiled

# Disabling garbage collection (in Julia) during parse gets more consistent
# results.  The mean time doesn't seem to change much and is sometimes faster
# with gc enabled somehow. Anyway, let's not disable it so we can benchmark
# more real-world.

# gc_disable()
min_time = minimum([@elapsed parse(grammar, sample_json) for i in 1:REPEAT])
# gc_enable()
# gc()

# Show time and memory allocated for one run. At the moment, should be around
# 70 ms and 15 MB.

println("For one pass over the text: @time parse(grammar, sample_json")
@time parse(grammar, sample_json)
println()

kb = length(sample_json)/1024.
ms = min_time*1e3
kbps =(length(sample_json)/min_time/1024.)
@printf "Min time to parse %.1f KB is %.0f ms which is %.0f KB/s\n" kb ms kbps
