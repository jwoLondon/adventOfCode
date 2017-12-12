{- Crossing the bridge, you've barely reached the other side of the stream when
   a program comes up to you, clearly in distress. "It's my child process," she
   says, "he's gotten lost in an infinite grid!"

   Fortunately for her, you have plenty of experience with infinite grids.
   Unfortunately for you, it's a hex grid.

   The hexagons ("hexes") in this grid are aligned such that adjacent hexes can
   be found to the north, northeast, southeast, south, southwest, and northwest:

     \ n  /
   nw +--+ ne
     /    \
   -+      +-
     \    /
   sw +--+ se
     / s  \

   You have the path the child process took. Starting where he started, you need
   to determine the fewest number of steps required to reach him. (A "step" means
   to move from the hex you are in to any adjacent hex.)

   For example:

   ne,ne,ne is 3 steps away.
   ne,ne,sw,sw is 0 steps away (back where you started).
   ne,ne,s,s is 2 steps away (se,se).
   se,sw,se,sw,sw is 3 steps away (s,s,sw).

   --- Part Two ---

   How many steps away is the furthest he ever got from his starting position?
-}


module D11_2017 exposing (..)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat)


main : Program Never Model Msg
main =
    aoc "data/d11_2017.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)



{- Can represent a hex position conveniently as position along three axes. -}


type alias Position =
    ( Int, Int, Int )


part1 : String -> Int
part1 =
    parse
        >> List.foldl move ( 0, 0, 0 )
        >> distFromOrigin


part2 : String -> Int
part2 =
    parse
        >> List.scanl move ( 0, 0, 0 )
        >> List.map distFromOrigin
        >> List.maximum
        >> Maybe.withDefault 0


move : Position -> Position -> Position
move ( x0, y0, z0 ) ( x1, y1, z1 ) =
    ( x0 + x1, y0 + y1, z0 + z1 )


distFromOrigin : Position -> Int
distFromOrigin ( x, y, z ) =
    (abs x + abs y + abs z) // 2


parse : String -> List Position
parse =
    let
        toVect instr =
            case instr of
                "n" ->
                    ( 0, 1, 1 )

                "ne" ->
                    ( 1, 1, 0 )

                "se" ->
                    ( 1, 0, -1 )

                "s" ->
                    ( 0, -1, -1 )

                "sw" ->
                    ( -1, -1, 0 )

                "nw" ->
                    ( -1, 0, 1 )

                _ ->
                    ( 0, 0, 0 ) |> Debug.log "Unknown instruction"
    in
    String.split ","
        >> List.map toVect
