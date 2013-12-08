"""Tests to show that the benchmarks we based our speed optimizations on are
still valid"""

module test_benchmarks

using Util
# using Benchmark
using Base.Test

# TODO: Implement convergence in Benchmark necessetating some LiveStates functionality too.

percentdiff(a, b) = 100 * (a-b)/a

# def test_lists_vs_dicts():
"""See what's faster at int key lookup: dicts or lists."""
list_time = @timeit :(x -> l[1 + x % 9000]) :(l = linspace(1,10000, 10001))
dict_time = @timeit :(x -> d[1 + int(x % 9000)]) :(d = {x => 1. + x for x in 1:10000})

@show list_time, dict_time
@test list_time < dict_time

# Arrays are faster than dictionaries, duh.
# But surprisingly not by much
# (list_time,dict_time) => (487.932448,772.362224)


end
