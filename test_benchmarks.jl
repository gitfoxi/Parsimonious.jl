"""Tests to show that the benchmarks we based our speed optimizations on are
still valid"""

module test_benchmarks

reload("Util.jl")
using Util
# using Benchmark
using Base.Test

# TODO: Implement convergence in Benchmark necessetating some LiveStates functionality too.

macro calltimes(ex, times)
    quote
        for i in 1:$times
            $ex
        end
    end
end

function setup_lookup(n, m)
    dict = Dict{(Int, Int), Float64}()
    for i in 1:n
        for j in 1:m
            dict[(i, j)] = 0.0
        end
    end
    return dict
end

function do_baseline(n::Int, m::Int)
    for i in 1:n
        for j in 1:m
        end
    end
end

function bench_lookup(n::Int, m::Int, dict)
    for i in 1:n
        for j in 1:m
            dict[(i, j)]
        end
    end
end

function setup_lookup(n)
    dict = Dict{Int, Float64}()
    for i in 1:n
        dict[i] = 0.0
    end
    return dict
end

function do_baseline(n::Int)
    for i in 1:n
    end
end

function bench_lookup(n::Int, dict)
    for i in 1:n
        dict[i]
    end
end


dict = setup_lookup(40000)
baseline = mean([(@elapsed (@calltimes do_baseline(40000) 3)) for i in 1:5]) / 3.
time_lookup = mean([(@elapsed (@calltimes bench_lookup(40000, dict) 3)) for i in 1:5]) / 3.

@printf "Julia int-key lookup: %.2f milliseconds\n" (time_lookup - baseline) * 1000.

dict = setup_lookup(200, 200)
baseline = mean([(@elapsed (@calltimes do_baseline(200, 200) 3)) for i in 1:5]) / 3.
time_lookup = mean([(@elapsed (@calltimes bench_lookup(200, 200, dict) 3)) for i in 1:5]) / 3.

@printf "Julia tuple-key lookup: %.2f milliseconds\n" (time_lookup - baseline) * 1000.

tcount = 0
type Trex
    depth::Int
    child

    function Trex(depth)
        global tcount
        tcount += 1

        if(depth > 0)
            new(depth, (Trex(depth - 1), Trex(depth - 1)))
        else
            new(depth, nothing)
        end
    end
end

function reset_tcount()
    global tcount
    tcount = 0
end
reset_tcount()

@time Trex(10)
@show tcount

end
