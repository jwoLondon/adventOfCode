{- Walking along the memory banks of the stream, you find a small village that is
   experiencing a little confusion: some programs can't communicate with each other.

   Programs in this village communicate using a fixed system of pipes. Messages
   are passed between programs using these pipes, but most programs aren't connected
   to each other directly. Instead, programs pass messages between each other until
   the message reaches the intended recipient.

   For some reason, though, some of these messages aren't ever reaching their
   intended recipient, and the programs suspect that some pipes are missing.
   They would like you to investigate.

   You walk through the village and record the ID of each program and the IDs with
   which it can communicate directly (your puzzle input). Each program has one or
   more programs with which it can communicate, and these pipes are bidirectional;
   if 8 says it can communicate with 11, then 11 will say it can communicate with 8.

   You need to figure out how many programs are in the group that contains
   program ID 0.

   For example, suppose you go door-to-door like a travelling salesman and record
   the following list:

   0 <-> 2
   1 <-> 1
   2 <-> 0, 3, 4
   3 <-> 2, 4
   4 <-> 2, 3, 6
   5 <-> 6
   6 <-> 4, 5

   In this example, the following programs are in the group that contains program ID 0:

   Program 0 by definition.
   Program 2, directly connected to program 0.
   Program 3 via program 2.
   Program 4 via program 2.
   Program 5 via programs 6, then 4, then 2.
   Program 6 via programs 4, then 2.

   Therefore, a total of 6 programs are in this group; all but program 1, which
   has a pipe that connects it to itself.

   How many programs are in the group that contains program ID 0?

   --- Part Two ---

   There are more programs than just the ones in the group containing program ID 0.
   The rest of them have no way of reaching that group, and still might have no
   way of reaching each other.

   A group is a collection of programs that can all communicate via pipes either
   directly or indirectly. The programs you identified just a moment ago are all
   part of the same group. Now, they would like you to determine the total number
   of groups.

   In the example above, there were 2 groups: one consisting of programs 0,2,3,4,5,6,
   and the other consisting solely of program 1.

   How many groups are there in total?
-}


module D12_2017 exposing (..)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, toInt)
import Dict exposing (Dict)
import Regex exposing (regex)
import Set exposing (Set)


type alias Neighbours =
    Dict Int (List Int)


type alias Group =
    Set Int


main : Program Never Model Msg
main =
    aoc "data/d12_2017.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 =
    List.map parseLine
        >> Dict.fromList
        >> getGroup 0
        >> Set.size


part2 : List String -> Int
part2 input =
    let
        neighbours =
            input
                |> List.map parseLine
                |> Dict.fromList
    in
    List.foldl (\node groups -> Set.insert (getGroup node neighbours |> Set.toList) groups)
        Set.empty
        (Dict.keys neighbours)
        |> Set.size


getGroup : Int -> Neighbours -> Group
getGroup node neighbours =
    buildGroup (Dict.get node neighbours |> Maybe.withDefault [])
        (Set.singleton node)
        neighbours


buildGroup : List Int -> Group -> Neighbours -> Group
buildGroup linked group neighbours =
    case linked of
        [] ->
            group

        hd :: tl ->
            if Set.member hd group then
                buildGroup tl group neighbours
            else
                buildGroup (addUnique (Dict.get hd neighbours |> Maybe.withDefault []) tl)
                    (Set.insert hd group)
                    neighbours


addUnique : List Int -> List Int -> List Int
addUnique l1 l2 =
    l1 ++ l2 |> Set.fromList |> Set.toList


parseLine : String -> ( Int, List Int )
parseLine str =
    let
        toLinks : List String -> ( Int, List Int )
        toLinks strs =
            case List.map toInt strs of
                hd :: tl ->
                    ( hd, tl )

                _ ->
                    ( 0, [] ) |> Debug.log "Bad input"
    in
    str
        |> Regex.find Regex.All (Regex.regex "(\\d+)")
        |> List.concatMap .submatches
        |> List.filterMap identity
        |> toLinks
