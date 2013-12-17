
export pause, @assert_raises, @timeit, @p_str, @p_mstr, @dq_str, @sq_str, @s_mstr

macro p_str(s) s end
macro p_mstr(s) s end
macro dq_str(s) "\"" * s * "\"" end
macro sq_str(s) "'" * s * "'" end
macro s_mstr(s) strip(lstrip(s))  end


function pause(msg="PAUSED")
    println(msg)
    readline()
end

macro assert_raises(exc, code)
    quote
        try
            $(code)
        catch e
            isa(e, $(exc)) && return true
        end
        return false
    end
end

macro timeit(run, setup, n)
    quote
        eval($(setup))
        f = eval($(run))
        [f(i) for i in 1:100] # compile
        # benchmark(f, string($(run)), 500000)["AverageWall"][1]
        # n = 500000
        td = Array(Uint64, $n)
        q = 0.
        for (i, x) in zip(1:$n, rand(Uint64, $n))
            t_start = time_ns()
            q += f(x)
            t_end = time_ns()
            td[i] = t_end - t_start
        end
        println(q)  # so it can't optimize out no matter how smart
        mean(td)
    end
end

