{- ADD CHALLENGE INSTRUCTIONS HERE -}


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
getGroup seed neighbours =
    buildGroup (Dict.get seed neighbours |> Maybe.withDefault [])
        (Set.singleton seed)
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
