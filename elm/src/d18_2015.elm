{- After the million lights incident, the fire code has gotten stricter: now, at
   most ten thousand lights are allowed. You arrange them in a 100x100 grid.

   Never one to let you down, Santa again mails you instructions on the ideal lighting
   configuration. With so few lights, he says, you'll have to resort to animation.

   Start by setting your lights to the included initial configuration (your puzzle
   input). A # means "on", and a . means "off".

   Then, animate your grid in steps, where each step decides the next configuration
   based on the current one. Each light's next state (either on or off) depends
   on its current state and the current states of the eight lights adjacent to it
   (including diagonals). Lights on the edge of the grid might have fewer than eight
   neighbors; the missing ones always count as "off".

   For example, in a simplified 6x6 grid, the light marked A has the neighbors numbered
   1 through 8, and the light marked B, which is on an edge, only has the neighbors
   marked 1 through 5:

   1B5...
   234...
   ......
   ..123.
   ..8A4.
   ..765.

   The state a light should have next is based on its current state (on or off)
   plus the number of neighbors that are on:

   A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
   A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
   All of the lights update simultaneously; they all consider the same current state
   before moving to the next.

   Here's a few steps from an example configuration of another 6x6 grid:

   Initial state:
   .#.#.#
   ...##.
   #....#
   ..#...
   #.#..#
   ####..

   After 1 step:
   ..##..
   ..##.#
   ...##.
   ......
   #.....
   #.##..

   After 2 steps:
   ..###.
   ......
   ..###.
   ......
   .#....
   .#....

   After 3 steps:
   ...#..
   ......
   ...#..
   ..##..
   ......
   ......

   After 4 steps:
   ......
   ......
   ..##..
   ..##..
   ......
   ......

   After 4 steps, this example has four lights on.

   In your grid of 100x100 lights, given your initial configuration, how many lights
   are on after 100 steps?

   --- Part Two ---

   You flip the instructions over; Santa goes on to point out that this is all just
   an implementation of Conway's Game of Life. At least, it was, until you notice
   that something's wrong with the grid of lights you bought: four lights, one in
   each corner, are stuck on and can't be turned off. The example above will actually
   run like this:

   Initial state:
   ##.#.#
   ...##.
   #....#
   ..#...
   #.#..#
   ####.#

   After 1 step:
   #.##.#
   ####.#
   ...##.
   ......
   #...#.
   #.####

   After 2 steps:
   #..#.#
   #....#
   .#.##.
   ...##.
   .#..##
   ##.###

   After 3 steps:
   #...##
   ####.#
   ..##.#
   ......
   ##....
   ####.#

   After 4 steps:
   #.####
   #....#
   ...#..
   .##...
   #.....
   #.#..#

   After 5 steps:
   ##.###
   .##..#
   .##...
   .##...
   #.#...
   ##...#

   After 5 steps, this example now has 17 lights on.

   In your grid of 100x100 lights, given your initial configuration, but with the
   four corners always in the on state, how many lights are on after 100 steps?
-}


module D18_2015 exposing (Lights, Location, emptyGrid, evolve, evolvedCell, evolvedGrid, gridSize, lightAt, main, nextLocation, numNeighbours, numberOn, parse, part1, part2, setLight)

import AdventOfCode exposing (Model, Msg, aoc, outFormat)
import Array exposing (Array)


type alias Lights =
    Array Int


type alias Location =
    ( Int, Int )


main : Program () Model Msg
main =
    aoc "../data/d18_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 input =
    let
        lights =
            parse input
    in
    evolve False 100 lights |> numberOn


part2 : List String -> Int
part2 input =
    let
        lights =
            parse input
    in
    evolve True 100 lights |> numberOn


gridSize : Int
gridSize =
    100


emptyGrid : Lights
emptyGrid =
    Array.repeat (gridSize * gridSize) 0


evolve : Bool -> Int -> Lights -> Lights
evolve isPart2 numEvolutions lights =
    let
        lights2 =
            if isPart2 then
                lights
                    |> setLight ( 0, 0 ) 1
                    |> setLight ( 0, gridSize - 1 ) 1
                    |> setLight ( gridSize - 1, gridSize - 1 ) 1
                    |> setLight ( gridSize - 1, 0 ) 1

            else
                lights
    in
    if numEvolutions <= 0 then
        lights2

    else
        evolve isPart2 (numEvolutions - 1) (evolvedGrid ( 0, 0 ) lights2 emptyGrid)


evolvedGrid : Location -> Lights -> Lights -> Lights
evolvedGrid ( x, y ) prevLights newLights =
    if x >= gridSize || y >= gridSize then
        newLights

    else
        evolvedGrid
            (nextLocation ( x, y ))
            prevLights
            (setLight ( x, y ) (evolvedCell lightAt ( x, y ) prevLights) newLights)


evolvedCell : (Location -> Lights -> Int) -> Location -> Lights -> Int
evolvedCell lightFn location lights =
    let
        neighbours =
            numNeighbours lightFn location lights
    in
    if lightFn location lights == 1 then
        if neighbours == 2 || neighbours == 3 then
            1

        else
            0

    else if neighbours == 3 then
        1

    else
        0


nextLocation : Location -> Location
nextLocation ( x, y ) =
    if x < gridSize - 1 then
        ( x + 1, y )

    else
        ( 0, y + 1 )


numberOn : Lights -> Int
numberOn lights =
    Array.toList lights |> List.sum


lightAt : Location -> Lights -> Int
lightAt ( x, y ) lights =
    if (x < 0) || (y < 0) || (x >= gridSize) || (y >= gridSize) then
        0

    else
        Maybe.withDefault 0 <| Array.get (y * gridSize + x) lights


setLight : Location -> Int -> Lights -> Lights
setLight ( x, y ) value lights =
    Array.set (y * gridSize + x) value lights


numNeighbours : (Location -> Lights -> Int) -> Location -> Lights -> Int
numNeighbours lightFn ( x, y ) lights =
    lightAt ( x - 1, y - 1 ) lights
        + lightFn ( x, y - 1 ) lights
        + lightFn ( x + 1, y - 1 ) lights
        + lightFn ( x - 1, y ) lights
        + lightFn ( x + 1, y ) lights
        + lightFn ( x - 1, y + 1 ) lights
        + lightFn ( x, y + 1 ) lights
        + lightFn ( x + 1, y + 1 ) lights


parse : List String -> Lights
parse input =
    let
        digits chr =
            if chr == '#' then
                1

            else
                0
    in
    input
        |> List.foldr (++) ""
        |> String.toList
        |> List.map digits
        |> Array.fromList
