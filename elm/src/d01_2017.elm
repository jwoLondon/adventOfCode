{- You're standing in a room with "digitization quarantine" written in LEDs along
   one wall. The only door is locked, but it includes a small interface. "Restricted
   Area - Strictly No Digitized Users Allowed."

   It goes on to explain that you may only leave by solving a captcha to prove you're
   not a human. Apparently, you only get one millisecond to solve the captcha: too
   fast for a normal human, but it feels like hours to you.

   The captcha requires you to review a sequence of digits (your puzzle input) and
   find the sum of all digits that match the next digit in the list. The list is
   circular, so the digit after the last digit is the first digit in the list.

   For example:

   * 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second
     digit and the third digit (2) matches the fourth digit.
   * 1111 produces 4 because each digit (all 1) matches the next.
   * 1234 produces 0 because no digit matches the next.
   * 91212129 produces 9 because the only digit that matches the next one is the
     last digit, 9.

   What is the solution to your captcha?

   --- Part Two ---

   You notice a progress bar that jumps to 50% completion. Apparently, the door
   isn't yet satisfied, but it did emit a star as encouragement. The instructions
   change:

   Now, instead of considering the next digit, it wants you to consider the digit
   halfway around the circular list. That is, if your list contains 10 items, only
   include a digit in your sum if the digit 10/2 = 5 steps forward matches it.
   Fortunately, your list has an even number of elements.

   For example:

   * 1212 produces 6: the list contains 4 items, and all four digits match the digit
     2 items ahead.
   * 1221 produces 0, because every comparison is between a 1 and a 2.
   * 123425 produces 4, because both 2s match each other, but no other digit has
     a match.
   * 123123 produces 12.
   * 12131415 produces 4.

   What is the solution to your new captcha?
-}


module D01_2017 exposing (captcha, main, matchedVal, part1, part2, rotate)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat, toInt)


main : Program () Model Msg
main =
    aoc "../data/d01_2017.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)


part1 : String -> Int
part1 input =
    captcha 1 input


part2 : String -> Int
part2 input =
    captcha (String.length input // 2) input


captcha : Int -> String -> Int
captcha numRotations digitText =
    let
        digits =
            String.toList digitText |> List.map (\c -> toInt (String.fromChar c))
    in
    List.map2 matchedVal digits (rotate numRotations digits)
        |> List.filterMap identity
        |> List.sum


matchedVal : comparable -> comparable -> Maybe comparable
matchedVal val1 val2 =
    if val1 == val2 then
        Just val1

    else
        Nothing


rotate : Int -> List a -> List a
rotate n xs =
    let
        pivot =
            modBy (List.length xs) n
    in
    List.drop pivot xs ++ List.take pivot xs