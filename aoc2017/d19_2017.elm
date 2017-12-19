{- ADD CHALLENGE INSTRUCTIONS HERE -}


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
