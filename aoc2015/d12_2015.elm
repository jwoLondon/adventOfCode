{- Santa's Accounting-Elves need help balancing the books after a recent order.
   Unfortunately, their accounting software uses a peculiar storage format. That's
   where you come in.

   They have a JSON document which contains a variety of things: arrays ([1,2,3]),
   objects ({"a":1, "b":2}), numbers, and strings. Your first job is to simply find
   all of the numbers throughout the document and add them together.

   For example:

   [1,2,3] and {"a":2,"b":4} both have a sum of 6.
   [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
   {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
   [] and {} both have a sum of 0.
   You will not encounter any strings containing numbers.

   What is the sum of all numbers in the document?

   --- Part Two ---

   Uh oh - the Accounting-Elves have realized that they double-counted everything
   red.

   Ignore any object (and all of its children) which has any property with the value
    "red". Do this only for objects ({...}), not arrays ([...]).

   [1,2,3] still has a sum of 6.
   [1,{"c":"red","b":2},3] now has a sum of 4, because the middle object is ignored.
   {"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0, because the entire structure is ignored.
   [1,"red",5] has a sum of 6, because "red" in an array has no effect.
-}


module D12_2015 exposing (JsVal(..), countAllNumbers, countNonRedNumbers, jsValDecoder, main, part1, part2, toSum)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat, split, toInt)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)


main : Program () Model Msg
main =
    aoc "data/d12_2015.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)


part1 : String -> Int
part1 input =
    countAllNumbers input


part2 : String -> Int
part2 input =
    case D.decodeString jsValDecoder input of
        Ok jsVal ->
            countNonRedNumbers jsVal 0

        Err msg ->
            0


countAllNumbers : String -> Int
countAllNumbers text =
    List.foldl toSum 0 (split "[^0-9-]+" text)


toSum : String -> Int -> Int
toSum text =
    (+) (toInt text)


countNonRedNumbers : JsVal -> Int -> Int
countNonRedNumbers jsVal total =
    case jsVal of
        JsInt num ->
            total + num

        JsArray list ->
            total + List.foldl countNonRedNumbers 0 list

        JsObject object ->
            if List.member (JsString "red") (Dict.values object) then
                total

            else
                total + List.foldl countNonRedNumbers 0 (Dict.values object)

        JsFloat flt ->
            -- Ignore floats for this question as none present in input
            total

        JsString str ->
            total

        JsNull ->
            total


{-| JsVal allows every JSON type to be represented.
See <https://stackoverflow.com/questions/40825493/elm-decoding-unknown-json-structure>
-}
type JsVal
    = JsString String
    | JsInt Int
    | JsFloat Float
    | JsArray (List JsVal)
    | JsObject (Dict String JsVal)
    | JsNull


{-| Decodes a JSON type and stores it as one of the JsVal types. Note that this
single type can be an array or object which can contain nested types within.
See <https://stackoverflow.com/questions/40825493/elm-decoding-unknown-json-structure>
-}
jsValDecoder : Decoder JsVal
jsValDecoder =
    D.oneOf
        [ D.map JsString D.string
        , D.map JsInt D.int
        , D.map JsFloat D.float
        , D.list (D.lazy (\_ -> jsValDecoder)) |> D.map JsArray
        , D.dict (D.lazy (\_ -> jsValDecoder)) |> D.map JsObject
        , D.null JsNull
        ]
