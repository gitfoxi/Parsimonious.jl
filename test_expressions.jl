
# I'm a module so that I can re-include Expressions every time so I get updates to the code
module TestExpressions
include("expressions.jl")

using Base.Test


import Expressions: Literal, Regex, Sequence, OneOf, Not, Optional, ZeroOrMore, OneOrMore, Expression
# require("test")


@test 1==1

function len_eq(node, length)
    node_length = node._end - node.start
    if node_length != length
        println("node_length: ", node_length, " length", length)
    end
    return node_length == length
end

# test_context("Testing Expressions")

# Literal tests
@test len_eq(match(Literal("hello"), "ehello", 2), 5)
# Not sure if it's possible to have a match-nothing literal
# @test len_eq(match(Literal(""), ""), 0)
@test len_eq(match(Literal("a"), "a"), 1)

# Regex tests
@test len_eq(match(Regex("hello*"), "hellooo"), 7)  # *
@test len_eq(match(Regex("hello*"), "hellooono"), 7)  # *
@test_throws match(Regex("hello*"), "goodbye")  # no match
@test len_eq(match(Regex("hello", options="i"), "HELLO"), 5)

# test_sequence:
@test len_eq(match(Sequence(Regex("hi*"), Literal("lo"), Regex(".ingo")), "hiiiilobingo1234"), 12)  # succeed
@test_throws len_eq(match(Sequence(Regex("hi*"), Literal("lo"), Regex(".ingo")), "ohiiiilobingo1234"), 12)  # fail
@test_throws match(Sequence(Regex("hi*"), Literal("lo"), Regex(".ingo")), "hiiiilobing")  # don"t
@test len_eq(match(Sequence(Regex("hi*")), ">hiiii", 2), 5)  # non-0 pos
#
## test_one_of:
@test len_eq(match(OneOf(Literal("aaa"), Literal("bb")), "aaa"), 3)  # first alternative
@test len_eq(match(OneOf(Literal("aaa"), Literal("bb")), "bbaaa"), 2)  # second
@test_throws match(OneOf(Literal("aaa"), Literal("bb")), "aa")  # no match
#
## test_not:
@test len_eq(match(Not(Regex(".")), ""), 0)  # match
@test_throws match(Not(Regex(".")), "Hi")  # don"t
#
## test_optional:
@test len_eq(match(Sequence(Optional(Literal("a")), Literal("b")), "b"), 1)  # contained expr fails
#        len_eq(Sequence(Optional(Literal("a")), Literal("b")).match("ab"), 2)  # contained expr succeeds
#
## test_zero_or_more:
#        len_eq(ZeroOrMore(Literal("b")).match(""), 0)  # zero
#        len_eq(ZeroOrMore(Literal("b")).match("bbb"), 3)  # more
#
#        len_eq(Regex("^").match(""), 0)  # Validate the next test.
#
#        # Try to make it loop infinitely using a zero-length contained expression:
#        len_eq(ZeroOrMore(Regex("^")).match(""), 0)
#
## test_one_or_more:
#        len_eq(OneOrMore(Literal("b")).match("b"), 1)  # one
#        len_eq(OneOrMore(Literal("b")).match("bbb"), 3)  # more
#        len_eq(OneOrMore(Literal("b"), min=3).match("bbb"), 3)  # with custom min; success
#        assert_raises(ParseError, OneOrMore(Literal("b"), min=3).match, "bb")  # with custom min; failure
#        len_eq(OneOrMore(Regex("^")).match("bb"), 0)  # attempt infinite loop

end
