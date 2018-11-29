{- As you walk through the door, a glowing humanoid shape yells in your direction.
   "You there! Your state appears to be idle. Come help us repair the corruption
   in this spreadsheet - if we take another millisecond, we'll have to display an
   hourglass cursor!"

   The spreadsheet consists of rows of apparently-random numbers. To make sure the
   recovery process is on the right track, they need you to calculate the spreadsheet's
   checksum. For each row, determine the difference between the largest value and
   the smallest value; the checksum is the sum of all of these differences.

   For example, given the following spreadsheet:

   5 1 9 5
   7 5 3
   2 4 6 8

   The first row's largest and smallest values are 9 and 1, and their difference is 8.
   The second row's largest and smallest values are 7 and 3, and their difference is 4.
   The third row's difference is 6.

   In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

   What is the checksum for the spreadsheet in your puzzle input?

   --- Part Two ---

   "Great work; looks like we're on the right track after all. Here's a star for
   your effort." However, the program seems a little worried. Can programs be worried?

   "Based on what we're seeing, it looks like all the User wanted is some information
   about the evenly divisible values in the spreadsheet. Unfortunately, none of
   us are equipped for that kind of calculation - most of us specialize in bitwise
   operations."

   It sounds like the goal is to find the only two numbers in each row where one
   evenly divides the other - that is, where the result of the division operation
   is a whole number. They would like you to find those numbers on each line, divide
   them, and add up each line's result.

   For example, given the following spreadsheet:

   5 9 2 8
   9 4 7 3
   3 8 6 5

   In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
   In the second row, the two numbers are 9 and 3; the result is 3.
   In the third row, the result is 2.
   In this example, the sum of the results would be 4 + 3 + 2 = 9.

   What is the sum of each row's result in your puzzle input?
-}


module D02_2017 exposing (checksum, diff, divis, main, parseLine, part1, part2)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, parseInt, selectLargest, toInt, whitespace)
import Parser exposing ((|.), (|=), Parser)


main : Program () Model Msg
main =
    aoc "../data/d02_2017.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 =
    checksum diff


part2 : List String -> Int
part2 =
    checksum divis


checksum : (List Int -> Int) -> List String -> Int
checksum check input =
    List.map parseLine input
        |> List.map check
        |> List.sum


diff : List Int -> Int
diff numbers =
    Maybe.map2 (-) (List.maximum numbers) (List.minimum numbers)
        |> Maybe.withDefault -99999


divis : List Int -> Int
divis nums =
    let
        divPairs ( n, ns ) =
            case List.filter (\x -> modBy x n == 0) ns of
                [ x ] ->
                    Just ( n, x )

                _ ->
                    Nothing
    in
    selectLargest nums
        |> List.filterMap divPairs
        |> List.map (\( a, b ) -> a // b)
        |> List.sum


parseLine : String -> List Int
parseLine txt =
    case txt |> Parser.run parseInts of
        Ok xs ->
            xs

        Err err ->
            let
                _ =
                    Debug.log "I had trouble parsing " err
            in
            []


parseInts : Parser (List Int)
parseInts =
    let
        intListParse revInts =
            Parser.oneOf
                [ nextInt |> Parser.andThen (\n -> intListParse (n :: revInts))
                , Parser.succeed (List.reverse revInts)
                ]

        nextInt =
            Parser.succeed identity
                |. whitespace
                |= parseInt
    in
    Parser.succeed identity
        |= Parser.andThen (\n -> intListParse [ n ]) parseInt



{- Retained for reference only. The regex equivalent to the elm parser:


   parseLine : String -> List Int
   parseLine text =
       --  List.map toInt (Regex.split Regex.All (regex "\\s+") text)
       Regex.split (Regex.fromString "\\s+" |> Maybe.withDefault Regex.never) text
           |> List.map toInt
-}
