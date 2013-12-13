"""Benchmarks for Parsimonious

Run these with ``nosetests parsimonious/tests/bench.py``. They don't run during
normal test runs because they're not tests--they don't assert anything. Also,
they're a bit slow.

These differ from the ones in test_benchmarks in that these are meant to be
compared from revision to revision of Parsimonious to make sure we're not
getting slower. test_benchmarks simply makes sure our choices among
implementation alternatives remain valid.

"""
# These aren't really tests, as they don't assert anything, but I found myself
# rewriting nose's discovery and selection bits, so why not just use nose?

module benchmarks

# import gc -- have gc_disable(), gc_enable(), gc()
# from timeit import repeat

using Util
using Grammars
export parse, grammar, json

# def test_not_really_json_parsing():
"""As a baseline for speed, parse some JSON.

I have no reason to believe that JSON is a particularly representative or
revealing grammar to test with. Also, this is a naive, unoptimized,
incorrect grammar, so don't use it as a basis for comparison with other
parsers. It's just meant to compare across versions of Parsimonious.

"""

father = """{
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
    }"""

more_fathers = join(repeat([father], inner=[60]), ",")

json = """{"fathers" : [""" * more_fathers * "]}"

notjsonspec = """
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

# notjsonspec = unescape_string(notjsonspec)
println("SPEC:\n", notjsonspec)
grammar = Grammar(notjsonspec)

# This is what it takes to debug stupid quoting issues
parse(grammar.exprs["chars"], """hello""")
parse(grammar.exprs["number"], """12""")
parse(grammar.exprs["dq"], "\"")
parse(grammar.exprs["string"], (s""" "hello" """))

# These number and repetition values seem to keep results within 5% of the
# difference between min and max. We get more consistent results running a
# bunch of single-parse tests and taking the min rather than upping the
# NUMBER and trying to stomp out the outliers with averaging.
NUMBER = 1
REPEAT = 5

parse(grammar, father)
parse(grammar, json)

# Disabling garbage collection during parse gets more consistent results.
# The mean time doesn't seem to change much and is sometimes faster with gc
# enabled somehow.

# gc_disable()
# @timed -> (return vale, time in seconds, memory in bytes)
min_time = minimum([@elapsed parse(grammar, json) for i in 1:5])
# gc_enable()
# gc()
@time parse(grammar, json)

@printf "Min time to parse %.1f KB is %.0f ms which is %.0f KB/s\n" length(json) min_time*1e3 (length(json)/min_time/1024.)

# @timeit :(parse(grammar, json)) :() 1000

end

"""
total_seconds = min(repeat(lambda: grammar.parse(json),
                           lambda: gc.enable(),  # so we take into account how we treat the GC
                           repeat=REPEAT,
                           number=NUMBER))
seconds_each = total_seconds / NUMBER

kb = len(json) / 1024.0
print 'Took %.3fs to parse %.1fKB: %.0fKB/s.' % (seconds_each,
                                                 kb,
                                                 kb / seconds_each)
"""
