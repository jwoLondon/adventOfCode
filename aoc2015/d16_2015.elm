{- Your Aunt Sue has given you a wonderful gift, and you'd like to send her a thank
    you card. However, there's a small problem: she signed it "From, Aunt Sue".

    You have 500 Aunts named "Sue".

    So, to avoid sending the card to the wrong person, you need to figure out which
    Aunt Sue (which you conveniently number 1 to 500, for sanity) gave you the gift.
    You open the present and, as luck would have it, good ol' Aunt Sue got you a
    My First Crime Scene Analysis Machine! Just what you wanted. Or needed, as the
    case may be.

    The My First Crime Scene Analysis Machine (MFCSAM for short) can detect a few
    specific compounds in a given sample, as well as how many distinct kinds of those
   compounds there are. According to the instructions, these are what the MFCSAM
   can detect:

    children, by human DNA age analysis.
    cats. It doesn't differentiate individual breeds.
    Several seemingly random breeds of dog: samoyeds, pomeranians, akitas, and vizslas.
    goldfish. No other kinds of fish.
    trees, all in one group.
    cars, presumably by exhaust or gasoline or something.
    perfumes, which is handy, since many of your Aunts Sue wear a few kinds.

    In fact, many of your Aunts Sue have many of these. You put the wrapping from
    the gift into the MFCSAM. It beeps inquisitively at you a few times and then
    prints out a message on ticker tape:

    children: 3
    cats: 7
    samoyeds: 2
    pomeranians: 3
    akitas: 0
    vizslas: 0
    goldfish: 5
    trees: 3
    cars: 2
    perfumes: 1

    You make a list of the things you can remember about each Aunt Sue. Things missing
    from your list aren't zero - you simply don't remember the value.

    What is the number of the Sue that got you the gift?

    --- Part Two ---

    As you're about to send the thank you note, something in the MFCSAM's instructions
    catches your eye. Apparently, it has an outdated retroencabulator, and so the
    output from the machine isn't exact values - some of them indicate ranges.

    In particular, the cats and trees readings indicates that there are greater than
    that many (due to the unpredictable nuclear decay of cat dander and tree pollen),
    while the pomeranians and goldfish readings indicate that there are fewer than
    that many (due to the modial interaction of magnetoreluctance).

    What is the number of the real Aunt Sue?
-}


module D16_2015 exposing (SelectionRule, Sue, addSue, couldBe, couldMatch1, couldMatch2, donor, main, parse, parseLine, part1, part2)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, submatches, toInt)
import Dict exposing (Dict)


type alias Sue =
    { name : String
    , characteristics : Dict String Int
    }


type alias SelectionRule =
    Dict String Int -> String -> Int -> Bool


main : Program () Model Msg
main =
    aoc "data/d16_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> String
part1 input =
    let
        sues =
            parse input
    in
    case List.head <| List.filter (couldBe couldMatch1 donor) sues of
        Nothing ->
            "No Sues match criteria"

        Just sue ->
            sue.name


part2 : List String -> String
part2 input =
    let
        sues =
            parse input
    in
    case List.head <| List.filter (couldBe couldMatch2 donor) sues of
        Nothing ->
            "No Sues match criteria"

        Just sue ->
            sue.name


donor : Sue
donor =
    Dict.empty
        |> Dict.insert "children" 3
        |> Dict.insert "cats" 7
        |> Dict.insert "samoyeds" 2
        |> Dict.insert "pomeranians" 3
        |> Dict.insert "akitas" 0
        |> Dict.insert "vizslas" 0
        |> Dict.insert "goldfish" 5
        |> Dict.insert "trees" 3
        |> Dict.insert "cars" 2
        |> Dict.insert "perfumes" 1
        |> Sue "donor"


couldBe : SelectionRule -> Sue -> Sue -> Bool
couldBe couldMatch sueA sueB =
    let
        matched =
            Dict.filter (couldMatch sueB.characteristics) sueA.characteristics
    in
    Dict.size matched == Dict.size sueA.characteristics


couldMatch1 : SelectionRule
couldMatch1 characteristics characteristic value =
    case Dict.get characteristic characteristics of
        Nothing ->
            True

        Just numItems ->
            numItems == value


couldMatch2 : SelectionRule
couldMatch2 characteristics characteristic value =
    case Dict.get characteristic characteristics of
        Nothing ->
            True

        Just numItems ->
            case characteristic of
                "cat" ->
                    numItems > value

                "trees" ->
                    numItems > value

                "pomeranians" ->
                    numItems < value

                "goldfish" ->
                    numItems < value

                _ ->
                    numItems == value


parse : List String -> List Sue
parse input =
    List.foldl parseLine [] input


parseLine : String -> List Sue -> List Sue
parseLine text sues =
    let
        regex =
            "(Sue \\d+): (\\w+): (\\d+)[,] (\\w+): (\\d+)[,] (\\w+): (\\d+)"
    in
    case submatches regex text of
        [ Just name, Just t1, Just n1, Just t2, Just n2, Just t3, Just n3 ] ->
            addSue name t1 (toInt n1) t2 (toInt n2) t3 (toInt n3) :: sues

        _ ->
            sues


addSue : String -> String -> Int -> String -> Int -> String -> Int -> Sue
addSue name t1 n1 t2 n2 t3 n3 =
    Dict.empty
        |> Dict.insert t1 n1
        |> Dict.insert t2 n2
        |> Dict.insert t3 n3
        |> Sue name
