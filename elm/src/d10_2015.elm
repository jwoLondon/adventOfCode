{- Today, the Elves are playing a game called look-and-say. They take turns making
   sequences by reading aloud the previous sequence and using that reading as the
   next sequence. For example, 211 is read as "one two, two ones", which becomes
   1221 (1 2, 2 1s).

   Look-and-say sequences are generated iteratively, using the previous value as
   input for the next step. For each step, take the previous value, and replace
   each run of digits (like 111) with the number of digits (3) followed by the
   digit itself (1).

   For example:

   1 becomes 11 (1 copy of digit 1).
   11 becomes 21 (2 copies of digit 1).
   21 becomes 1211 (one 2 followed by one 1).
   1211 becomes 111221 (one 1, one 2, and two 1s).
   111221 becomes 312211 (three 1s, two 2s, and one 1).
   Starting with the digits in your puzzle input, apply this process 40 times.
   What is the length of the result?

   --- Part Two ---

   Neat, right? You might also enjoy hearing John Conway talking about this sequence
   (that's Conway of Conway's Game of Life fame).

   Now, starting again with the digits in your puzzle input, apply this process
   50 times. What is the length of the new result?
-}


module D10_2015 exposing (iteratedLookSay, main, part1, part2, say)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat)
import Regex


main : Program () Model Msg
main =
    aoc "../data/d10_2015.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)


part1 : String -> Int
part1 =
    iteratedLookSay 40 >> String.length


part2 : String -> Int
part2 =
    iteratedLookSay 50 >> String.length


iteratedLookSay : Int -> String -> String
iteratedLookSay count look =
    if count == 0 then
        look

    else
        iteratedLookSay (count - 1) (say look)


say : String -> String
say look =
    Regex.replace
        (Regex.fromString "(\\d)\\1*" |> Maybe.withDefault Regex.never)
        (\m -> (String.length m.match |> String.fromInt) ++ String.left 1 m.match)
        look
