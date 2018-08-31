{- In years past, the holiday feast with your family hasn't gone so well. Not everyone
   gets along! This year, you resolve, will be different. You're going to find the
   optimal seating arrangement and avoid all those awkward conversations.

   You start by writing up a list of everyone invited and the amount their happiness
   would increase or decrease if they were to find themselves sitting next to each
   other person. You have a circular table that will be just big enough to fit
    everyone comfortably, and so each person will have exactly two neighbors.

   For example, suppose you have only four attendees planned, and you calculate
   their potential happiness as follows:

   Alice would gain 54 happiness units by sitting next to Bob.
   Alice would lose 79 happiness units by sitting next to Carol.
   Alice would lose 2 happiness units by sitting next to David.
   Bob would gain 83 happiness units by sitting next to Alice.
   Bob would lose 7 happiness units by sitting next to Carol.
   Bob would lose 63 happiness units by sitting next to David.
   Carol would lose 62 happiness units by sitting next to Alice.
   Carol would gain 60 happiness units by sitting next to Bob.
   Carol would gain 55 happiness units by sitting next to David.
   David would gain 46 happiness units by sitting next to Alice.
   David would lose 7 happiness units by sitting next to Bob.
   David would gain 41 happiness units by sitting next to Carol.

   Then, if you seat Alice next to David, Alice would lose 2 happiness units (because
   David talks so much), but David would gain 46 happiness units (because Alice is
   such a good listener), for a total change of 44.

   If you continue around the table, you could then seat Bob next to Alice (Bob
   gains 83, Alice gains 54). Finally, seat Carol, who sits next to Bob (Carol
   gains 60, Bob loses 7) and David (Carol gains 55, David gains 41). The arrangement
   looks like this:

        +41 +46
   +55   David    -2
   Carol       Alice
   +60    Bob    +54
        -7  +83
   After trying every other seating arrangement in this hypothetical scenario, you
   find that this one is the most optimal, with a total change in happiness of 330.

   What is the total change in happiness for the optimal seating arrangement of
   the actual guest list?

   --- Part Two ---

   In all the commotion, you realize that you forgot to seat yourself. At this point,
   you're pretty apathetic toward the whole thing, and your happiness wouldn't really
   go up or down regardless of who you sit next to. You assume everyone else would
   be just as ambivalent about sitting next to you, too.

   So, add yourself to the list, and give all happiness relationships that involve
   you a score of 0.

   What is the total change in happiness for the optimal seating arrangement that
   actually includes yourself?
-}


module D13_2015 exposing (NeighbourTable, addUnique, main, makeCycle, maxHappiness, parse, parseLine, part1, part2, peopleIn, totalH)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, permutations, toInt)
import Dict exposing (Dict)


type alias NeighbourTable =
    Dict ( String, String ) Int


main : Program () Model Msg
main =
    aoc "data/d13_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 input =
    let
        neighbourTable =
            parse input

        seatingPlans =
            makeCycle <| permutations (peopleIn neighbourTable)
    in
    maxHappiness seatingPlans 0 neighbourTable


part2 : List String -> Int
part2 input =
    let
        neighbourTable =
            parse input

        seatingPlans =
            makeCycle <| permutations <| "me" :: peopleIn neighbourTable
    in
    maxHappiness seatingPlans 0 neighbourTable


maxHappiness : List (List String) -> Int -> NeighbourTable -> Int
maxHappiness seatingPlans maxH neighbourTable =
    case seatingPlans of
        [] ->
            maxH

        hd :: tl ->
            maxHappiness tl (max maxH (totalH hd 0 neighbourTable)) neighbourTable


totalH : List String -> Int -> NeighbourTable -> Int
totalH seatingPlan total neighbourTable =
    case seatingPlan of
        [] ->
            total

        p1 :: p2 :: tl ->
            let
                d =
                    Maybe.withDefault 0 (Dict.get ( p1, p2 ) neighbourTable)
                        + Maybe.withDefault 0 (Dict.get ( p2, p1 ) neighbourTable)
            in
            totalH (p2 :: tl) (total + d) neighbourTable

        _ :: [] ->
            total


{-| Provides a full set of people found in the given neighbours table.
-}
peopleIn : NeighbourTable -> List String
peopleIn neighbourTable =
    let
        extract =
            \( p1, p2 ) -> \_ -> \items -> addUnique p1 items
    in
    Dict.foldl extract [] neighbourTable


addUnique : a -> List a -> List a
addUnique item list =
    if List.member item list then
        list

    else
        item :: list


parse : List String -> NeighbourTable
parse input =
    List.foldl parseLine Dict.empty input


parseLine : String -> NeighbourTable -> NeighbourTable
parseLine nText neighbourTable =
    case String.slice 0 -1 nText |> String.words of
        [ p1, "would", gainOrLose, units, "happiness", "units", "by", "sitting", "next", "to", p2 ] ->
            let
                hChange =
                    if gainOrLose == "gain" then
                        toInt units

                    else
                        -1 * toInt units
            in
            Dict.insert ( p1, p2 ) hChange neighbourTable

        _ ->
            neighbourTable


{-| Adds the first element of each list to the end, making a closed cycle.
-}
makeCycle : List (List a) -> List (List a)
makeCycle lists =
    let
        addHeadToTail list =
            case List.head list of
                Nothing ->
                    []

                Just hd ->
                    List.append list [ hd ]
    in
    List.map addHeadToTail lists
