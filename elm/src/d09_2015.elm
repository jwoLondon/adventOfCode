{- Every year, Santa manages to deliver all of his presents in a single night.

   This year, however, he has some new locations to visit; his elves have provided
   him the distances between every pair of locations. He can start and end at any
   two (different) locations he wants, but he must visit each location exactly once.
   What is the shortest distance he can travel to achieve this?

   For example, given the following distances:

   London to Dublin = 464
   London to Belfast = 518
   Dublin to Belfast = 141

   The possible routes are therefore:

   Dublin -> London -> Belfast = 982
   London -> Dublin -> Belfast = 605
   London -> Belfast -> Dublin = 659
   Dublin -> Belfast -> London = 659
   Belfast -> Dublin -> London = 605
   Belfast -> London -> Dublin = 982

   The shortest of these is London -> Dublin -> Belfast = 605, and so the answer
   is 605 in this example.

   What is the distance of the shortest route?

   --- Part Two ---

   The next year, just to show off, Santa decides to take the route with the longest
   distance instead.

   He can still start and end at any two (different) locations he wants, and he
   still must visit each location exactly once.

   For example, given the distances above, the longest route would be 982 via (for
   example) Dublin -> London -> Belfast.

   What is the distance of the longest route?
-}


module D09_2015 exposing (DistanceTable, addUnique, bestDistance, locations, main, parseLine, part1, part2, totalDist)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, permutations, toInt)
import Dict exposing (Dict)


main : Program () Model Msg
main =
    aoc "../data/d09_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 input =
    let
        distanceTable =
            List.foldl parseLine Dict.empty input
    in
    bestDistance min (locations distanceTable |> permutations) 99999 distanceTable


part2 : List String -> Int
part2 input =
    let
        distanceTable =
            List.foldl parseLine Dict.empty input
    in
    bestDistance max (locations distanceTable |> permutations) 0 distanceTable


type alias DistanceTable =
    Dict ( String, String ) Int


bestDistance : (Int -> Int -> Int) -> List (List String) -> Int -> DistanceTable -> Int
bestDistance op journeys bestDist distanceTable =
    case journeys of
        [] ->
            bestDist

        hd :: tl ->
            bestDistance op tl (op bestDist (totalDist hd 0 distanceTable)) distanceTable


{-| Provides a full set of unique locations found in the given distance table.
-}
locations : DistanceTable -> List String
locations =
    let
        extract =
            \( loc1, loc2 ) -> \_ -> \items -> addUnique loc1 items
    in
    Dict.foldl extract []


addUnique : a -> List a -> List a
addUnique item list =
    if List.member item list then
        list

    else
        item :: list


totalDist : List String -> Int -> DistanceTable -> Int
totalDist journeys total distanceTable =
    case journeys of
        [] ->
            total

        loc1 :: loc2 :: tl ->
            let
                d =
                    Maybe.withDefault 0 (Dict.get ( loc1, loc2 ) distanceTable)
            in
            totalDist (loc2 :: tl) (total + d) distanceTable

        _ :: [] ->
            total


parseLine : String -> DistanceTable -> DistanceTable
parseLine dText distanceTable =
    case String.words dText of
        [ place1, "to", place2, "=", dist ] ->
            Dict.insert ( place1, place2 ) (toInt dist) distanceTable
                |> Dict.insert ( place2, place1 ) (toInt dist)

        _ ->
            distanceTable
