#
# Correctness Tests
#

using Base.Test
# Stealing your sweet testrunner, thanks DataFrames.
# using DataFrames
using Parsimonious

my_tests = [
    "test/test_benchmarks.jl",
    "test/test_expressions.jl",
    # test_firmware.jl
    "test/test_grammars.jl",
    "test/test_nodes.jl"
]

println("Running tests:")

for my_test in my_tests
    println(" * $(my_test)")
    include(my_test)
end
