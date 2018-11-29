{- You find a program trying to generate some art. It uses a strange process that
   involves repeatedly enhancing the detail of an image through a set of rules.

   The image consists of a two-dimensional square grid of pixels that are either
   on (#) or off (.). The program always begins with this pattern:

   .#.
   ..#
   ###

   Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to have a size of 3.

   Then, the program repeats the following process:

   If the size is evenly divisible by 2, break the pixels up into 2x2 squares, and
   convert each 2x2 square into a 3x3 square by following the corresponding
   enhancement rule. Otherwise, the size is evenly divisible by 3; break the pixels
   up into 3x3 squares, and convert each 3x3 square into a 4x4 square by following
   the corresponding enhancement rule.

   Because each square of pixels is replaced by a larger one, the image gains pixels
   and so its size increases.

   The artist's book of enhancement rules is nearby (your puzzle input); however,
   it seems to be missing rules. The artist explains that sometimes, one must rotate
   or flip the input pattern to find a match. (Never rotate or flip the output pattern,
   though.) Each pattern is written concisely: rows are listed as single units,
   ordered top-down, and separated by slashes. For example, the following rules
   correspond to the adjacent patterns:

   ../.#  =  ..
             .#

                   .#.
   .#./..#/###  =  ..#
                   ###

                           #..#
   #..#/..../#..#/.##.  =  ....
                           #..#
                           .##.

   When searching for a rule to use, rotate and flip the pattern as necessary.
   For example, all of the following patterns match the same rule:

   .#.   .#.   #..   ###
   ..#   #..   #.#   ..#
   ###   ###   ##.   .#.

   Suppose the book contained the following two rules:

   ../.# => ##./#../...
   .#./..#/### => #..#/..../..../#..#

   As before, the program begins with this pattern:

   .#.
   ..#
   ###

   The size of the grid (3) is not divisible by 2, but it is divisible by 3. It
   divides evenly into a single square; the square matches the second rule,
   which produces:

   #..#
   ....
   ....
   #..#

   The size of this enhanced grid (4) is evenly divisible by 2, so that rule is
   used. It divides evenly into four squares:

   #.|.#
   ..|..
   --+--
   ..|..
   #.|.#

   Each of these squares matches the same rule (../.# => ##./#../...), three of
   which require some flipping and rotation to line up with the rule. The output
   for the rule is the same in all four cases:

   ##.|##.
   #..|#..
   ...|...
   ---+---
   ##.|##.
   #..|#..
   ...|...

   Finally, the squares are joined into a new grid:

   ##.##.
   #..#..
   ......
   ##.##.
   #..#..
   ......

   Thus, after 2 iterations, the grid contains 12 pixels that are on.

   How many pixels stay on after 5 iterations?

   --- Part Two ---

   How many pixels stay on after 18 iterations?
-}


module D21_2017 exposing (Image, Rules, count, enhance, flatten, grow, main, parse, parseLine, part1, part2, start)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, toInt)
import Dict exposing (Dict)
import Grid exposing (Grid)


type alias Rules =
    Dict String Image


type alias Image =
    Grid Char


main : Program () Model Msg
main =
    aoc "../data/d21_2017.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


start : Image
start =
    Grid.fromList ' '
        [ [ '.', '#', '.' ]
        , [ '.', '.', '#' ]
        , [ '#', '#', '#' ]
        ]


part1 : List String -> Int
part1 input =
    let
        rules =
            parse input
    in
    List.foldl (\_ img -> grow rules img) start (List.range 1 5)
        |> count


part2 : List String -> Int
part2 input =
    let
        rules =
            parse input
    in
    List.foldl (\_ img -> grow rules img) start (List.range 1 18)
        |> count


count : Image -> Int
count =
    Grid.toList
        >> List.filter (\c -> c == '#')
        >> List.length


grow : Rules -> Image -> Image
grow rules img =
    let
        get location =
            Grid.get location img |> Maybe.withDefault ' '

        indices n im =
            List.range 0 (Grid.colCount im // n - 1) |> List.map ((*) n)

        joinImageRow =
            List.foldl (\im cols -> cols ++ Grid.toCols im) []
                >> Grid.fromList ' '
                >> Grid.transpose
                >> Grid.toRows
    in
    if modBy 2 (Grid.rowCount img) == 0 then
        let
            imgRow r =
                List.map
                    (\c ->
                        Grid.fromList ' '
                            [ [ get ( r, c ), get ( r, c + 1 ) ]
                            , [ get ( r + 1, c ), get ( r + 1, c + 1 ) ]
                            ]
                    )
                    (indices 2 img)
                    |> List.map (enhance rules)
        in
        List.map imgRow (indices 2 img)
            |> List.foldl (\iRow iRows -> iRows ++ joinImageRow iRow) []
            |> Grid.fromList ' '

    else if modBy 3 (Grid.rowCount img) == 0 then
        let
            imgRow r =
                List.map
                    (\c ->
                        Grid.fromList ' '
                            [ [ get ( r, c ), get ( r, c + 1 ), get ( r, c + 2 ) ]
                            , [ get ( r + 1, c ), get ( r + 1, c + 1 ), get ( r + 1, c + 2 ) ]
                            , [ get ( r + 2, c ), get ( r + 2, c + 1 ), get ( r + 2, c + 2 ) ]
                            ]
                    )
                    (indices 3 img)
                    |> List.map (enhance rules)
        in
        List.map (\r -> imgRow r) (indices 3 img)
            |> List.foldl (\iRow iRows -> iRows ++ joinImageRow iRow) []
            |> Grid.fromList ' '

    else
        img |> Debug.log "Unexpected grid size"


enhance : Rules -> Image -> Image
enhance rules img =
    case Dict.get (flatten img) rules of
        Just i ->
            i

        Nothing ->
            img


flatten : Image -> String
flatten =
    Grid.toList >> String.fromList


parse : List String -> Rules
parse =
    let
        matchVariations ( inImg, outImg ) =
            (++)
                [ ( flatten inImg, outImg )
                , ( flatten (inImg |> Grid.rotate), outImg )
                , ( flatten (inImg |> Grid.rotate |> Grid.rotate), outImg )
                , ( flatten (inImg |> Grid.rotate |> Grid.rotate |> Grid.rotate), outImg )
                , ( flatten (inImg |> Grid.flipRows), outImg )
                , ( flatten (inImg |> Grid.flipRows |> Grid.rotate), outImg )
                , ( flatten (inImg |> Grid.flipRows |> Grid.rotate |> Grid.rotate), outImg )
                , ( flatten (inImg |> Grid.flipRows |> Grid.rotate |> Grid.rotate |> Grid.rotate), outImg )
                ]
    in
    List.map parseLine
        >> List.foldl matchVariations []
        >> Dict.fromList


parseLine : String -> ( Image, Image )
parseLine text =
    let
        toImage =
            String.split "/"
                >> List.map String.toList
                >> Grid.fromList ' '
    in
    case String.split " => " text of
        [ a, b ] ->
            ( toImage a, toImage b )

        _ ->
            ( Grid.init 0 0 ' ', Grid.init 0 0 ' ' ) |> Debug.log "Bad input"
