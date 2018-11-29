{- Santa is delivering presents to an infinite two-dimensional grid of houses.
    He begins by delivering a present to the house at his starting location, and
    then an elf at the North Pole calls him via radio and tells him where to move
    next. Moves are always exactly one house to the north (^), south (v), east (>),
    or west (<). After each move, he delivers another present to the house at his
    new location.

    However, the elf back at the north pole has had a little too much eggnog, and
    so his directions are a little off, and Santa ends up visiting some houses
    more than once. How many houses receive at least one present?

    For example:

    > delivers presents to 2 houses: one at the starting location, and one to the
      east.
    ^>v< delivers presents to 4 houses in a square, including twice to the house
         at his starting/ending location.
    ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2
               houses.

    --- Part Two ---

    The next year, to speed up the process, Santa creates a robot version of himself,
    Robo-Santa, to deliver presents with him.

    Santa and Robo-Santa start at the same location (delivering two presents to the
    same starting house), then take turns moving based on instructions from the elf,
    who is eggnoggedly reading from the same script as the previous year.

    This year, how many houses receive at least one present?

    For example:

   ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa
      goes south.
   ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back
        where they started.
   ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction
              and Robo-Santa going the other.
-}


module D03_2015 exposing (Location, addToFreqTable, dropPresents, main, move, part1, part2, thinList)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat)
import Dict exposing (Dict)


main : Program () Model Msg
main =
    aoc "../data/d03_2015.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)


part1 : String -> Int
part1 instructions =
    Dict.empty
        |> dropPresents (String.toList instructions) ( 0, 0 )
        |> Dict.size


part2 : String -> Int
part2 instructions =
    let
        santaInstr =
            String.toList instructions |> thinList

        roboInstr =
            String.toList instructions |> List.drop 1 |> thinList
    in
    Dict.empty
        |> dropPresents santaInstr ( 0, 0 )
        |> dropPresents roboInstr ( 0, 0 )
        |> Dict.size


type alias Location =
    ( Int, Int )


thinList : List a -> List a
thinList list =
    if List.length list == 0 then
        []

    else
        List.take 1 list ++ thinList (List.drop 2 list)


dropPresents : List Char -> Location -> Dict Location Int -> Dict Location Int
dropPresents instructions location visits =
    case instructions of
        [] ->
            addToFreqTable location visits

        hd :: tl ->
            dropPresents tl (move hd location) (addToFreqTable location visits)


move : Char -> Location -> Location
move instruction position =
    let
        ( x, y ) =
            position
    in
    case instruction of
        '<' ->
            ( x - 1, y )

        '>' ->
            ( x + 1, y )

        'v' ->
            ( x, y + 1 )

        '^' ->
            ( x, y - 1 )

        _ ->
            ( x, y )


addToFreqTable : comparable -> Dict comparable Int -> Dict comparable Int
addToFreqTable item freqTable =
    if Dict.member item freqTable then
        Dict.update item (Maybe.map ((+) 1)) freqTable

    else
        Dict.insert item 1 freqTable
