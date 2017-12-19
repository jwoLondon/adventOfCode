{- Somehow, a network packet got lost and ended up here. It's trying to follow a
   routing diagram (your puzzle input), but it's confused about where to go.

   Its starting point is just off the top of the diagram. Lines (drawn with |, -,
   and +) show the path it needs to take, starting by going down onto the only
   line connected to the top of the diagram. It needs to follow this path until
   it reaches the end (located somewhere within the diagram) and stop there.

   Sometimes, the lines cross over each other; in these cases, it needs to continue
   going the same direction, and only turn left or right when there's no other option.
   In addition, someone has left letters on the line; these also don't change its
   direction, but it can use them to keep track of where it's been. For example:

        |
        |  +--+
        A  |  C
    F---|----E|--+
        |  |  |  D
        +B-+  +--+

   Given this diagram, the packet needs to take the following path:

   * Starting at the only line touching the top of the diagram, it must go down,
     pass through A, and continue onward to the first +.
   * Travel right, up, and right, passing through B in the process.
   * Continue down (collecting C), right, and up (collecting D).
   * Finally, go all the way left through E and stopping at F.

   Following the path to the end, the letters it sees on its path are ABCDEF.

   The little packet looks up at you, hoping you can help it find the way. What
   letters will it see (in the order it would see them) if it follows the path?
   (The routing diagram is very wide; make sure you view it without line wrapping.)

   --- Part Two ---

   The packet is curious how many steps it needs to go.

   For example, using the same routing diagram from the example above...

        |
        |  +--+
        A  |  C
    F---|--|-E---+
        |  |  |  D
        +B-+  +--+

   ...the packet would go:

   6 steps down (including the first line at the top of the diagram).
   3 steps right.
   4 steps up.
   3 steps right.
   4 steps down.
   3 steps right.
   2 steps up.
   13 steps left (including the F it stops on).

   This would result in a total of 38 steps.

   How many steps does the packet need to go?
-}


module D19_2017 exposing (..)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, toInt)
import Grid exposing (Grid)


main : Program Never Model Msg
main =
    aoc "data/d19_2017.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


type alias Location =
    ( Int, Int )


type alias Direction =
    ( Int, Int )


type alias History =
    ( List Char, Int )


part1 : List String -> String
part1 =
    landmarkHistory
        >> Tuple.first
        >> String.fromList


part2 : List String -> Int
part2 =
    landmarkHistory
        >> Tuple.second
        >> (+) 1


landmarkHistory : List String -> History
landmarkHistory input =
    let
        ( history, destination, _ ) =
            move (parse input)
                ( ( [], 0 ), start input, ( 1, 0 ) )
    in
    history


start : List String -> ( Int, Int )
start =
    List.head
        >> Maybe.andThen (String.indexes "|" >> List.head)
        >> Maybe.withDefault -1
        >> (,) 0


move : Grid Char -> ( History, Location, Direction ) -> ( History, Location, Direction )
move grid ( ( landmarks, steps ), ( r, c ), ( dy, dx ) ) =
    case Grid.get ( r, c ) grid of
        Just '+' ->
            if dx == 0 && (Grid.get ( r, c - 1 ) grid |> isValidMove) then
                move grid ( ( landmarks, steps + 1 ), ( r, c - 1 ), ( 0, -1 ) )
            else if dx == 0 && (Grid.get ( r, c + 1 ) grid |> isValidMove) then
                move grid ( ( landmarks, steps + 1 ), ( r, c + 1 ), ( 0, 1 ) )
            else if dy == 0 && (Grid.get ( r + 1, c ) grid |> isValidMove) then
                move grid ( ( landmarks, steps + 1 ), ( r + 1, c ), ( 1, 0 ) )
            else if dy == 0 && (Grid.get ( r - 1, c ) grid |> isValidMove) then
                move grid ( ( landmarks, steps + 1 ), ( r - 1, c ), ( -1, 0 ) )
            else
                ( ( landmarks, steps ), ( r, c ), ( 0, 0 ) ) |> Debug.log "Cannot move"

        Just '|' ->
            move grid ( ( landmarks, steps + 1 ), ( r + dy, c + dx ), ( dy, dx ) )

        Just '-' ->
            move grid ( ( landmarks, steps + 1 ), ( r + dy, c + dx ), ( dy, dx ) )

        Just ' ' ->
            ( ( landmarks, steps ), ( r, c ), ( 0, 0 ) ) |> Debug.log "Fallen off path"

        Nothing ->
            ( ( landmarks, steps ), ( r, c ), ( 0, 0 ) ) |> Debug.log "Fallen off edge"

        Just landmark ->
            if Grid.get ( r + dy, c + dx ) grid |> isValidMove then
                move grid ( ( landmarks ++ [ landmark ], steps + 1 ), ( r + dy, c + dx ), ( dy, dx ) )
            else
                ( ( landmarks ++ [ landmark ], steps ), ( r, c ), ( dy, dx ) )


isValidMove : Maybe Char -> Bool
isValidMove chr =
    chr /= Just ' ' && chr /= Nothing


parse : List String -> Grid Char
parse =
    List.map String.toList
        >> Grid.fromList ' '