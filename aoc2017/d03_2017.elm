{- You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

   Each square on the grid is allocated in a spiral pattern starting at a location
   marked 1 and then counting up while spiraling outward. For example, the first
   few squares are allocated like this:

   17  16  15  14  13
   18   5   4   3  12
   19   6   1   2  11
   20   7   8   9  10
   21  22  23---> ...

   While this is very space-efficient (no squares are skipped), requested data must
   be carried back to square 1 (the location of the only access port for this memory
   system) by programs that can only move up, down, left, or right. They always
   take the shortest path: the Manhattan Distance between the location of the data
   and square 1.

   For example:

   Data from square 1 is carried 0 steps, since it's at the access port.
   Data from square 12 is carried 3 steps, such as: down, left, left.
   Data from square 23 is carried only 2 steps: up twice.
   Data from square 1024 must be carried 31 steps.

   How many steps are required to carry the data from the square identified in your
   puzzle input all the way to the access port?

   --- Part Two ---

   As a stress test on the system, the programs here clear the grid and then store
   the value 1 in square 1. Then, in the same allocation order as shown above, they
   store the sum of the values in all adjacent squares, including diagonals.

   So, the first few squares' values are chosen as follows:

   Square 1 starts with the value 1.
   Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
   Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
   Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
   Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.

   Once a square is written, its value does not change. Therefore, the first few
   squares would receive the following values:

   147  142  133  122   59
   304    5    4    2   57
   330   10    1    1   54
   351   11   23   25   26
   362  747  806--->   ...

   What is the first value written that is larger than your puzzle input?
-}


module D03_2017 exposing (..)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat, toInt)
import Dict exposing (Dict)


type alias Memory =
    Dict Int Int


type alias Location =
    ( Int, Int )


main : Program Never Model Msg
main =
    aoc "data/d03_2017.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)


part1 : String -> Int
part1 =
    toInt >> spiralToLocation >> manhattanDistance


part2 : String -> Int
part2 =
    toInt >> stressTest (Dict.singleton 1 1)


stressTest : Memory -> Int -> Int
stressTest memory threshold =
    if valueAtHighestAddress memory > threshold then
        valueAtHighestAddress memory
    else
        stressTest (memory |> accumNeighbours (highestAddress memory + 1)) threshold


highestAddress : Memory -> Int
highestAddress memory =
    Dict.keys memory |> List.maximum |> Maybe.withDefault 1


valueAtHighestAddress : Memory -> Int
valueAtHighestAddress memory =
    Dict.get (highestAddress memory) memory |> Maybe.withDefault 0


accumNeighbours : Int -> Memory -> Memory
accumNeighbours n memory =
    let
        valueAt addr =
            Dict.get addr memory |> Maybe.withDefault 0

        ( x0, y0 ) =
            spiralToLocation n

        accum =
            (valueAt <| locationToSpiral ( x0 + 1, y0 ))
                + (valueAt <| locationToSpiral ( x0 + 1, y0 - 1 ))
                + (valueAt <| locationToSpiral ( x0, y0 - 1 ))
                + (valueAt <| locationToSpiral ( x0 - 1, y0 - 1 ))
                + (valueAt <| locationToSpiral ( x0 - 1, y0 ))
                + (valueAt <| locationToSpiral ( x0 - 1, y0 + 1 ))
                + (valueAt <| locationToSpiral ( x0, y0 + 1 ))
                + (valueAt <| locationToSpiral ( x0 + 1, y0 + 1 ))
    in
    Dict.insert n accum memory


locationToSpiral : ( Int, Int ) -> Int
locationToSpiral ( x, y ) =
    let
        ring =
            max (abs x) (abs y)

        corner =
            (2 * ring + 1) * (2 * ring + 1)
    in
    if y == ring then
        corner - ring + x
    else if x == -ring then
        corner - 3 * ring + y
    else if y == -ring then
        corner - 5 * ring - x
    else
        corner - 7 * ring - y


spiralToLocation : Int -> Location
spiralToLocation n =
    let
        ring =
            ceiling ((sqrt (toFloat n) - 1) / 2)

        corner =
            (2 * ring + 1) * (2 * ring + 1)
    in
    if n >= corner - 2 * ring then
        ( n - corner + ring, ring )
    else if n >= corner - 4 * ring then
        ( -ring, n - corner + 3 * ring )
    else if n >= corner - 6 * ring then
        ( corner - 5 * ring - n, -ring )
    else
        ( ring, corner - 7 * ring - n )


manhattanDistance : ( Int, Int ) -> Int
manhattanDistance ( x, y ) =
    abs x + abs y
