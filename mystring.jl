## core text I/O ##


import Base.getindex
import Base.print_escaped
export getindex, print_escaped, writemime

# TODO: All strings are substrings
# TODO: prev -- a backwards next
# TODO: string pointer

# Feature: [] indexes unicode by character
# Feature: iteration over unicode characters instead of indexes

s = "as中文"
for c in s
    println("c: ", c)
end

oldgetindex(a::Array{Uint8,1}, i::Int) = Base.getindex(a, i)
oldgetindex(a::Array{Int64,1}, i::Int) = Base.getindex(a, i)
oldgetindex(a::Array{Uint32,1}, i::Int) = Base.getindex(a, i)
oldgetindex(ci::(Char, Int), i::Int) = Base.getindex(ci, i)

function oldgetindex(s::UTF8String, i::Int)
    # potentially faster version
    # d = s.data
    # a::Uint32 = d[i]
    # if a < 0x80; return char(a); end
    # #if a&0xc0==0x80; return '\ufffd'; end
    # b::Uint32 = a<<6 + d[i+1]
    # if a < 0xe0; return char(b - 0x00003080); end
    # c::Uint32 = b<<6 + d[i+2]
    # if a < 0xf0; return char(c - 0x000e2080); end
    # return char(c<<6 + d[i+3] - 0x03c82080)

    d = s.data
    b = oldgetindex(d, i)
    if !Base.is_utf8_start(b)
        j = i-1
        while 0 < j && !Base.is_utf8_start(oldgetindex(d, j))
            j -= 1
        end
        if 0 < j && i <= j+Base.utf8_trailing[oldgetindex(d, j)+1] <= length(d)
            # b is a continuation byte of a valid UTF-8 character
            error("invalid UTF-8 character index")
        end
        return '\ufffd'
    end
    trailing = oldgetindex(Base.utf8_trailing, b+1)
    if length(d) < i + trailing
        return '\ufffd'
    end
    c::Uint32 = 0
    for j = 1:trailing+1
        c <<= 6
        c += oldgetindex(d, i)
        i += 1
    end
    c -= oldgetindex(Base.utf8_offset, trailing+1)
    char(c)
end

function print_escaped(io, s::String, esc::String)
    i = start(s)
    while !done(s,i)
        c, j = oldnext(s,i)
        c == '\0'       ? print(io, escape_nul(s,j)) :
        c == '\e'       ? print(io, "\\e") :
        c == '\\'       ? print(io, "\\\\") :
        in(c,esc)       ? print(io, '\\', c) :
        7 <= c <= 13    ? print(io, '\\', "abtnvfr"[int(c-6)]) :
        isprint(c)      ? print(io, c) :
        c <= '\x7f'     ? print(io, "\\x", hex(c, 2)) :
        c <= '\uffff'   ? print(io, "\\u", hex(c, need_full_hex(s,j) ? 4 : 2)) :
                          print(io, "\\U", hex(c, need_full_hex(s,j) ? 8 : 4))
        i = j
    end
end


oldnext(s::UTF8String, i::Int) = (oldgetindex(s,i), i+1+oldgetindex(Base.utf8_trailing,oldgetindex(s.data,i)+1))

function oldnextind(s::String, i::Integer)
    e = endof(s)
    if i < 1
        return 1
    end
    if i > e
        return i+1
    end
    for j = i+1:e
        if isvalid(s,j)
            return j
        end
    end
    oldgetindex(oldnext(s,e), 2) # out of range
end

#function display(x)
#    for i = length(displays):-1:1
#        try
#            return display(displays[i], x)
#        catch e
#            if !isa(e, MethodError)
#                rethrow()
#            end
#        end
#    end
#    throw(MethodError(display, (x,)))
#end
function writemime(io::IO, ::MIME"text/plain", v::AbstractVector)
    if isa(v, Ranges)
        show(io, v)
    else
        print(io, summary(v))
        if !isempty(v)
            println(io, ":")
            print_matrix(io, v)
        end
    end
end


function mygetindex(s::UTF8String, i::Int)
    ind = 1
    for x in 2:i
        ind = oldnextind(s, ind)
    end
    oldgetindex(s, ind)
end

getindex(s::UTF8String, i::Int) = mygetindex(s, i)
getindex(s::UTF8String, i::Integer) = mygetindex(s,int(i))

getindex(s::String, v::AbstractVector) =
    sprint(length(v), io->(for i in v write(io,oldgetindex(s,i)) end))

println(mygetindex(s, 4))
println(getindex(s, 4))
println(s[4])
# writemime(STDOUT, MIME"text/plain", s)

# Feature: [:] doesn't care if you overrun the bounds
# Feature: [:] doesn't waste memory, makes substrings
getindex{T<:Integer}(s::String, r::Range1{T}) = SubString(s, int(max(1,first(r))), int(min(endof(s),last(r))))
getindex(s::ASCIIString, r::Range1{Int}) =  SubString(s, int(max(1,first(r))), int(min(endof(s),last(r))))
# TODO: fixme
getindex(s::UTF8String, r::Range1{Int}) =  SubString(s, int(max(1,first(r))), int(chr2ind(s, min(length(s), last(r)))))

# TODO: slices like s[1:4] still don't index on
