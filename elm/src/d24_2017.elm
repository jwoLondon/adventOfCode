{- The CPU itself is a large, black building surrounded by a bottomless pit.
   Enormous metal tubes extend outward from the side of the building at regular
   intervals and descend down into the void. There's no way to cross, but you
   need to get inside.

   No way, of course, other than building a bridge out of the magnetic components
   strewn about nearby.

   Each component has two ports, one on each end. The ports come in all different
   types, and only matching types can be connected. You take an inventory of the
   components by their port types (your puzzle input). Each port is identified by
   the number of pins it uses; more pins mean a stronger connection for your bridge.
   A 3/7 component, for example, has a type-3 port on one side, and a type-7 port
   on the other.

   Your side of the pit is metallic; a perfect surface to connect a magnetic,
   zero-pin port. Because of this, the first port you use must be of type 0. It
   doesn't matter what type of port you end with; your goal is just to make the
   bridge as strong as possible.

   The strength of a bridge is the sum of the port types in each component. For
   example, if your bridge is made of components 0/3, 3/7, and 7/4, your bridge
   has a strength of 0+3 + 3+7 + 7+4 = 24.

   For example, suppose you had the following components:

   0/2
   2/2
   2/3
   3/4
   3/5
   0/1
   10/1
   9/10

   With them, you could make the following valid bridges:

   0/1
   0/1--10/1
   0/1--10/1--9/10
   0/2
   0/2--2/3
   0/2--2/3--3/4
   0/2--2/3--3/5
   0/2--2/2
   0/2--2/2--2/3
   0/2--2/2--2/3--3/4
   0/2--2/2--2/3--3/5

   (Note how, as shown by 10/1, order of ports within a component doesn't matter.
   However, you may only use each port on a component once.)

   Of these bridges, the strongest one is 0/1--10/1--9/10; it has a strength of
   0+1 + 1+10 + 10+9 = 31.

   What is the strength of the strongest bridge you can make with the components
   you have available?

   --- Part Two ---

   The bridge you've built isn't long enough; you can't jump the rest of the way.

   In the example above, there are two longest bridges:

   0/2--2/2--2/3--3/4
   0/2--2/2--2/3--3/5

   Of them, the one which uses the 3/5 component is stronger; its strength is
   0+2 + 2+2 + 2+3 + 3+5 = 19.

   What is the strength of the longest bridge you can make? If you can make
   multiple bridges of the longest length, pick the strongest one.
-}


module D24_2017 exposing (BridgeProp, Component, MaxFn, addDouble, available, join, main, maxSecond, newAnchor, parse, part1, part2, removeUsedDouble, strength, swap)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, toInt)
import Set exposing (Set)


main : Program () Model Msg
main =
    aoc "../data/d24_2017.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


type alias Component =
    ( Int, Int )


type alias BridgeProp =
    ( Int, Int )


type alias MaxFn comparable =
    comparable -> comparable -> comparable


part1 : List String -> Int
part1 =
    {- Use partition to optimise search by removing 'doubles' (e.g. 3/3 or 1/1) and
       adding their score if bridge contains their value somewhere along its length.
    -}
    parse
        >> Set.partition (\( a, b ) -> a /= b)
        >> join max 0 ( 0, 0 ) ( 0, 0 )
        >> Tuple.first


part2 : List String -> Int
part2 =
    parse
        >> Set.partition (\( a, b ) -> a /= b)
        >> join maxSecond 0 ( 0, 0 ) ( 0, 0 )
        >> Tuple.first


join : MaxFn BridgeProp -> Int -> BridgeProp -> BridgeProp -> ( Set Component, Set Component ) -> BridgeProp
join maxFn anchor ( stren, len ) maxVals ( components, doubles ) =
    let
        maxScore c =
            join maxFn
                (newAnchor anchor c)
                (maxFn maxVals (( stren + strength c, len + 1 ) |> addDouble c doubles))
                (maxFn maxVals ( stren + strength c, len + 1 ))
                ( Set.remove c components, removeUsedDouble c doubles )

        scores =
            available anchor components
                |> Set.map maxScore
    in
    maxFn maxVals (scores |> Set.foldl maxFn ( -1, -1 ))


addDouble : Component -> Set Component -> BridgeProp -> BridgeProp
addDouble ( a, b ) doubles ( str, len ) =
    if Set.member ( a, a ) doubles then
        ( str + 2 * a, len + 1 )

    else if Set.member ( b, b ) doubles then
        ( str + 2 * b, len + 1 )

    else
        ( str, len )


removeUsedDouble : Component -> Set Component -> Set Component
removeUsedDouble ( a, b ) =
    Set.remove ( a, a ) >> Set.remove ( b, b )


maxSecond : MaxFn BridgeProp
maxSecond pair1 pair2 =
    max (swap pair1) (swap pair2) |> swap


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


strength : Component -> Int
strength ( a, b ) =
    a + b


newAnchor : Int -> Component -> Int
newAnchor anchor component =
    if Tuple.first component == anchor then
        Tuple.second component

    else if Tuple.second component == anchor then
        Tuple.first component

    else
        -1 |> Debug.log "Cannot match component"


available : Int -> Set Component -> Set Component
available anchor =
    Set.filter (\c -> Tuple.first c == anchor || Tuple.second c == anchor)


parse : List String -> Set Component
parse =
    let
        toComp pair =
            case String.split "/" pair of
                [ a, b ] ->
                    ( toInt a, toInt b )

                _ ->
                    ( -1, -1 ) |> Debug.log "Bad input"
    in
    List.map toComp
        >> Set.fromList
