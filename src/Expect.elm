module Expect exposing (Expectation, pass, fail, getFailure, equal, notEqual, atMost, lessThan, greaterThan, atLeast, between, all, custom, true, false, onFail)

{-| A library to create `Expectation`s, which describe a claim to be tested.

## Quick Reference

* [`equal`](#equal) `(arg2 == arg1)`
* [`notEqual`](#notEqual) `(arg2 /= arg1)`
* [`lessThan`](#lessThan) `(arg2 < arg1)`
* [`atMost`](#atMost) `(arg2 <= arg1)`
* [`greaterThan`](#greaterThan) `(arg2 > arg1)`
* [`atLeast`](#atLeast) `(arg2 >= arg1)`
* [`between`](#between) `(arg1 <= arg3 <= arg2)`
* [`true`](#true) `(arg == True)`
* [`false`](#false) `(arg == False)`

## Basic Expectations

@docs Expectation, equal, notEqual

## Comparisons

@docs lessThan, atMost, greaterThan, atLeast, between

## Booleans

@docs true, false

## Customizing

@docs all, pass, fail, custom, onFail, getFailure
-}

import Test.Expectation
import String


{-| The result of a single test run: either a [`pass`](#pass) or a
[`fail`](#fail).
-}
type alias Expectation =
    Test.Expectation.Expectation


{-| Passes if the arguments are equal.

    Expect.equal 0 (List.length [])

    -- Passes because (0 == 0) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because the expected value didn't split the space in "Betty Botter"
    String.split " " "Betty Botter bought some butter"
        |> Expect.equal [ "Betty Botter", "bought", "some", "butter" ]

    {-

    [ "Betty", "Botter", "bought", "some", "butter" ]
    ╷
    │ Expect.equal
    ╵
    [ "Betty Botter", "bought", "some", "butter" ]

    -}
-}
equal : a -> a -> Expectation
equal =
    compareWith "Expect.equal" (==)


{-| Passes if the arguments are not equal.

    -- Passes because (11 /= 100) is True
    90 + 10
        |> Expect.notEqual 11


    -- Fails because (100 /= 100) is False
    90 + 10
        |> Expect.notEqual 100

    {-

    100
    ╷
    │ Expect.notEqual
    ╵
    100

    -}
-}
notEqual : a -> a -> Expectation
notEqual =
    compareWith "Expect.notEqual" (/=)


{-| Passes if the second argument is less than the first.

    Expect.lessThan 1 (List.length [])

    -- Passes because (0 < 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 < -1) is False
    List.length []
        |> Expect.lessThan -1


    {-

    0
    ╷
    │ Expect.lessThan
    ╵
    -1

    -}
-}
lessThan : comparable -> comparable -> Expectation
lessThan =
    compareWith "Expect.lessThan" (<)


{-| Passes if the second argument is less than or equal to the first.

    Expect.atMost 1 (List.length [])

    -- Passes because (0 <= 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 <= -3) is False
    List.length []
        |> Expect.atMost -3

    {-

    0
    ╷
    │ Expect.atMost
    ╵
    -3

    -}
-}
atMost : comparable -> comparable -> Expectation
atMost =
    compareWith "Expect.atMost" (<=)


{-| Passes if the second argument is greater than the first.

    Expect.greaterThan -2 List.length []

    -- Passes because (0 > -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 > 1) is False
    List.length []
        |> Expect.greaterThan 1

    {-

    0
    ╷
    │ Expect.greaterThan
    ╵
    1

    -}
-}
greaterThan : comparable -> comparable -> Expectation
greaterThan =
    compareWith "Expect.greaterThan" (>)


{-| Passes if the second argument is greater than or equal to the first.

    Expect.atLeast -2 (List.length [])

    -- Passes because (0 >= -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 >= 3) is False
    List.length []
        |> Expect.atLeast 3

    {-

    0
    ╷
    │ Expect.atLeast
    ╵
    3

    -}
-}
atLeast : comparable -> comparable -> Expectation
atLeast =
    compareWith "Expect.atLeast" (>=)


{-| Passes if the third argument is greater than or equal to the first, *and*
less than or equal to the second

    4 |> Expect.between 2 4

    -- Passes because (l <= 4 && 4 <= 4) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 >= 3) is False
    List.length []
        |> Expect.atLeast between 3 5

    {-

    0
    ╷
    │ Expect.between
    ╵
    3 and 5

    -}
-}
between : comparable -> comparable -> comparable -> Expectation
between lo hi val =
    if lo <= val && val <= hi then
        pass
    else
        fail <| reportFailure "Expect.between" (toString val) <| toString lo ++ " and " ++ toString hi


{-| Passes if the argument is 'True', and otherwise fails with the given message.

    Expect.true "Expected the list to be empty." (List.isEmpty [])

    -- Passes because (List.isEmpty []) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because List.isEmpty returns False, but we expect True.
    List.isEmpty [ 42 ]
        |> Expect.true "Expected the list to be empty."

    {-

    Expected the list to be empty.

    -}
-}
true : String -> Bool -> Expectation
true message bool =
    if bool then
        pass
    else
        fail message


{-| Passes if the argument is 'False', and otherwise fails with the given message.

    Expect.false "Expected the list not to be empty." (List.isEmpty [ 42 ])

    -- Passes because (List.isEmpty [ 42 ]) is False

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (List.isEmpty []) is True
    List.isEmpty []
        |> Expect.false "Expected the list not to be empty."

    {-

    Expected the list not to be empty.

    -}
-}
false : String -> Bool -> Expectation
false message bool =
    if bool then
        fail message
    else
        pass


{-| Expect many expectations to all pass. If the list provided is empty, the
test fails, because this is certainly a bug in your test and we don't want to
give you false confidence in your suite.

    fuzz (list string) "All lists of strings contain 'foo' and 'bar' <|
        all [ List.member "foo" >> true "didn't have 'foo'"
            , List.member "bar" >> true "didn't have 'bar'"
            ]
-}
all : List (a -> Expectation) -> a -> Expectation
all expectations actual =
    if List.isEmpty expectations then
        fail "Expect.all must not be given an empty list!"
    else
        expectations
            |> List.map ((|>) actual)
            |> List.filter ((/=) Test.Expectation.Pass)
            -- TODO show all failures not just the first
            |>
                List.head
            |> Maybe.withDefault pass


{-| Always passes.

    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)
    import Expect


    test "Json.Decode.int can decode the number 42." <|
        \() ->
            case decodeString int "42" of
                Ok _ ->
                    Expect.pass

                Err err ->
                    Expect.fail err
-}
pass : Expectation
pass =
    Test.Expectation.Pass


{-| Fails with the given message.

    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)
    import Expect


    test "Json.Decode.int can decode the number 42." <|
        \() ->
            case decodeString int "42" of
                Ok _ ->
                    Expect.pass

                Err err ->
                    Expect.fail err
-}
fail : String -> Expectation
fail =
    Test.Expectation.Fail ""


{-| Create a custom expectation. The logic lives in the provided function that
takes the expected and actual values and produces either `Err "an error
message"` indicating a failure or `Ok ()` indicating a pass.
-}
custom : (a -> b -> Result String ()) -> a -> b -> Expectation
custom f expected actual =
    case f expected actual of
        Ok () ->
            pass

        Err str ->
            fail <| reportFailure str (toString expected) (toString actual)


{-| Return `Nothing` if the given [`Expectation`](#Expectation) is a [`pass`](#pass).

If it is a [`fail`](#fail), return a record containing the failure message,
along with the given inputs if it was a fuzz test. (If no inputs were involved,
the record's `given` field will be `""`).

For example, if a fuzz test generates random integers, this might return
`{ message = "it was supposed to be positive", given = "-1" }`

    getFailure (Expect.fail "this failed")
    -- Just { message = "this failed", given = "" }

    getFailure (Expect.pass)
    -- Nothing
-}
getFailure : Expectation -> Maybe { given : String, message : String }
getFailure expectation =
    case expectation of
        Test.Expectation.Pass ->
            Nothing

        Test.Expectation.Fail given message ->
            Just { given = given, message = message }


{-| If the given expectation fails, replace its failure message with a custom one.

    "something"
        |> Expect.equal "something else"
        |> Expect.onFail "thought those two strings would be the same"
-}
onFail : String -> Expectation -> Expectation
onFail str expectation =
    case expectation of
        Test.Expectation.Pass ->
            expectation

        Test.Expectation.Fail given _ ->
            Test.Expectation.Fail given str


reportFailure : String -> String -> String -> String
reportFailure comparison expected actual =
    [ actual
    , "╷"
    , "│ " ++ comparison
    , "╵"
    , expected
    ]
        |> String.join "\n"


compareWith : String -> (a -> b -> Bool) -> b -> a -> Expectation
compareWith label compare expected actual =
    if compare actual expected then
        pass
    else
        fail (reportFailure label (toString expected) (toString actual))
