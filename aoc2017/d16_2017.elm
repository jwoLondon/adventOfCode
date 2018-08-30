{- You come upon a very unusual sight; a group of programs here appear to be dancing.

   There are sixteen programs in total, named a through p. They start by standing
   in a line: a stands in position 0, b stands in position 1, and so on until p,
   which stands in position 15.

   The programs' dance consists of a sequence of dance moves:

   * Spin, written sX, makes X programs move from the end to the front, but maintain
     their order otherwise. (For example, s3 on abcde produces cdeab).
   * Exchange, written xA/B, makes the programs at positions A and B swap places.
   * Partner, written pA/B, makes the programs named A and B swap places.

   For example, with only five programs standing in a line (abcde), they could do the following dance:

   s1, a spin of size 1: eabcd.
   x3/4, swapping the last two programs: eabdc.
   pe/b, swapping programs e and b: baedc.

   After finishing their dance, the programs end up in order baedc.

   You watch the dance for a while and record their dance moves (your puzzle input).
   In what order are the programs standing after their dance?

   --- Part Two ---

   Now that you're starting to get a feel for the dance moves, you turn your attention
   to the dance as a whole.

   Keeping the positions they ended up in from their previous dance, the programs
   perform it again and again: including the first dance, a total of one billion
   (1000000000) times.

   In the example above, their second dance would begin with the order baedc, and
   use the same dance moves:

   s1, a spin of size 1: cbaed.
   x3/4, swapping the last two programs: cbade.
   pe/b, swapping programs e and b: ceadb.

   In what order are the programs standing after their billion dances?
-}


module D16_2017 exposing (Move(..), Progs, dance, danceMove, exchange, findPeriod, main, parseLine, part1, part2, partner, spin, start)

import AdventOfCode exposing (Model, Msg, aoc, matches, multiLineInput, outFormat, toInt)
import Array exposing (Array)


main : Program () Model Msg
main =
    aoc "data/d16_2017.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)


type alias Progs =
    List Char


type Move
    = Spin Int
    | Exchange Int Int
    | Partner Char Char


start : Progs
start =
    "abcdefghijklmnop" |> String.toList


part1 : String -> String
part1 input =
    start
        |> dance (parseLine input)
        |> String.fromList


part2 : String -> String
part2 input =
    let
        moves =
            parseLine input

        numRepeats =
            modBy (findPeriod moves) 1000000000
    in
    List.foldl (\_ ps -> dance moves ps) start (List.range 1 numRepeats)
        |> String.fromList


dance : List Move -> Progs -> Progs
dance moves progs =
    List.foldl danceMove progs moves


danceMove : Move -> Progs -> Progs
danceMove move progs =
    case move of
        Spin n ->
            spin n progs

        Exchange n1 n2 ->
            exchange n1 n2 progs

        Partner p1 p2 ->
            partner p1 p2 progs


findPeriod : List Move -> Int
findPeriod moves =
    let
        matchStart n progs =
            if progs == start then
                n

            else
                matchStart (n + 1) (dance moves progs)
    in
    matchStart 1 (dance moves start)


partner : Char -> Char -> Progs -> Progs
partner p1 p2 prog =
    let
        swap p =
            if p == p1 then
                p2

            else if p == p2 then
                p1

            else
                p
    in
    List.map swap prog


exchange : Int -> Int -> Progs -> Progs
exchange n1 n2 prog =
    let
        arr =
            Array.fromList prog

        p1 =
            Array.get n1 arr |> Maybe.withDefault 'x'

        p2 =
            Array.get n2 arr |> Maybe.withDefault 'x'
    in
    arr |> Array.set n1 p2 |> Array.set n2 p1 |> Array.toList


spin : Int -> Progs -> Progs
spin n prog =
    let
        pivot =
            List.length prog - modBy (List.length prog) n
    in
    List.drop pivot prog ++ List.take pivot prog


parseLine : String -> List Move
parseLine =
    let
        toMove : List Move -> List (Maybe String) -> List Move
        toMove moves tokens =
            case List.take 3 tokens of
                [] ->
                    List.reverse moves

                [ Just "s", Just n, Nothing ] ->
                    toMove (Spin (toInt n) :: moves) (List.drop 3 tokens)

                [ Just "x", Just n1, Just n2 ] ->
                    toMove (Exchange (toInt n1) (toInt n2) :: moves) (List.drop 3 tokens)

                [ Just "p", Just p1, Just p2 ] ->
                    toMove
                        (Partner (p1 |> String.toList |> List.head |> Maybe.withDefault 'x')
                            (p2 |> String.toList |> List.head |> Maybe.withDefault 'x')
                            :: moves
                        )
                        (List.drop 3 tokens)

                _ ->
                    List.reverse moves |> Debug.log "Bad input"
    in
    matches "(s|x|p)(\\d+|[a-p])(?:/(\\d+|[a-p]))?"
        >> toMove []
