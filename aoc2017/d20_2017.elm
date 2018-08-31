{- Suddenly, the GPU contacts you, asking for help. Someone has asked it to
   simulate too many particles, and it won't be able to finish them all in time
   to render the next frame at this rate.

   It transmits to you a buffer (your puzzle input) listing each particle in
   order (starting with particle 0, then particle 1, particle 2, and so on). For
   each particle, it provides the X, Y, and Z coordinates for the particle's
   position (p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

   Each tick, all particles are updated simultaneously. A particle's properties
   are updated in the following order:

   Increase the X velocity by the X acceleration.
   Increase the Y velocity by the Y acceleration.
   Increase the Z velocity by the Z acceleration.
   Increase the X position by the X velocity.
   Increase the Y position by the Y velocity.
   Increase the Z position by the Z velocity.

   Because of seemingly tenuous rationale involving z-buffering, the GPU would like
   to know which particle will stay closest to position <0,0,0> in the long term.
   Measure this using the Manhattan distance, which in this situation is simply
   the sum of the absolute values of a particle's X, Y, and Z position.

   For example, suppose you are only given two particles, both of which stay
   entirely on the X-axis (for simplicity). Drawing the current states of particles
   0 and 1 (in that order) with an adjacent a number line and diagram of current
   X positions (marked in parenthesis), the following would take place:

   p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
   p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

   p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
   p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

   p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
   p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

   p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
   p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)

   At this point, particle 1 will never be closer to <0,0,0> than particle 0,
   and so, in the long run, particle 0 will stay closest.

   Which particle will stay closest to position <0,0,0> in the long term?

   --- Part Two ---

   To simplify the problem further, the GPU would like to remove any particles
   that collide. Particles collide if their positions ever exactly match. Because
   particles are updated simultaneously, more than two particles can collide at
   the same time and place. Once particles collide, they are removed and cannot
   collide with anything else after that tick.

   For example:

   p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
   p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
   p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
   p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>

   p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
   p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
   p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
   p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>

   p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
   p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
   p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
   p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>

   ------destroyed by collision------
   ------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
   ------destroyed by collision------                      (3)
   p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>

   In this example, particles 0, 1, and 2 are simultaneously destroyed at the
   time and place marked X. On the next tick, particle 3 passes through unharmed.

   How many particles are left after all collisions are resolved?
-}


module D20_2017 exposing (Particle, incTime, magnitude, main, pCompare, parseLine, part1, part2, removeCollisions, simulate)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, submatches, toInt)


main : Program () Model Msg
main =
    aoc "data/d20_2017.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


type alias Particle =
    ( ( Int, Int, Int ), ( Int, Int, Int ), ( Int, Int, Int ) )


part1 : List String -> Int
part1 =
    List.map parseLine
        >> List.indexedMap (\a b -> ( a, b ))
        >> List.sortWith pCompare
        >> List.head
        >> Maybe.map Tuple.first
        >> Maybe.withDefault -1


part2 : List String -> Int
part2 =
    List.map parseLine
        >> List.indexedMap (\a b -> ( a, b ))
        >> simulate 100


pCompare : ( Int, Particle ) -> ( Int, Particle ) -> Order
pCompare ( _, ( p0, v0, a0 ) ) ( _, ( p1, v1, a1 ) ) =
    if magnitude a0 < magnitude a1 then
        LT

    else if magnitude a0 > magnitude a1 then
        GT

    else if magnitude v0 < magnitude v1 then
        LT

    else if magnitude v0 > magnitude v1 then
        GT

    else if magnitude p0 < magnitude p1 then
        LT

    else if magnitude p0 > magnitude p1 then
        GT

    else
        EQ


magnitude : ( Int, Int, Int ) -> Int
magnitude ( x, y, z ) =
    abs x + abs y + abs z


simulate : Int -> List ( Int, Particle ) -> Int
simulate steps =
    if steps <= 0 then
        List.length

    else
        removeCollisions []
            >> incTime
            >> simulate (steps - 1)


removeCollisions : List ( Int, Particle ) -> List ( Int, Particle ) -> List ( Int, Particle )
removeCollisions uncollided iParticles =
    let
        collide ( _, ( p0, _, _ ) ) ( _, ( p1, _, _ ) ) =
            p0 == p1
    in
    case iParticles of
        [] ->
            uncollided

        p :: ps ->
            if List.any (collide p) ps then
                removeCollisions uncollided (List.filter (not << collide p) ps)

            else
                removeCollisions (p :: uncollided) ps


incTime : List ( Int, Particle ) -> List ( Int, Particle )
incTime =
    let
        tick ( index, ( ( px0, py0, pz0 ), ( vx0, vy0, vz0 ), ( ax, ay, az ) ) ) =
            let
                ( vx, vy, vz ) =
                    ( vx0 + ax, vy0 + ay, vz0 + az )

                ( px, py, pz ) =
                    ( px0 + vx, py0 + vy, pz0 + vz )
            in
            ( index, ( ( px, py, pz ), ( vx, vy, vz ), ( ax, ay, az ) ) )
    in
    List.map tick


parseLine : String -> Particle
parseLine input =
    let
        vectors =
            submatches "(-?\\d+)"
                >> List.filterMap identity
                >> List.map toInt
    in
    case vectors input of
        [ px, py, pz, vx, vy, vz, ax, ay, az ] ->
            ( ( px, py, pz ), ( vx, vy, vz ), ( ax, ay, az ) )

        _ ->
            ( ( 0, 0, 0 ), ( 0, 0, 0 ), ( 0, 0, 0 ) ) |> Debug.log "Bad input"
