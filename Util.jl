
module Util

export pause, @assert_raises

function pause()
    println("PAUSED")
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

end
