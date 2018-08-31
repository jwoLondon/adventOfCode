{- This year, Santa brought little Bobby Tables a set of wires and bitwise logic
   gates! Unfortunately, little Bobby is a little under the recommended age range,
   and he needs help assembling the circuit.

   Each wire has an identifier (some lowercase letters) and can carry a 16-bit
   signal (a number from 0 to 65535). A signal is provided to each wire by a gate,
   another wire, or some specific value. Each wire can only get a signal from one
   source, but can provide its signal to multiple destinations. A gate provides
   no signal until all of its inputs have a signal.

   The included instructions booklet describes how to connect the parts together:
   * x AND y -> z means to connect wires x and y to an AND gate, and then connect
     its output to wire z.

   For example:

   * 123 -> x means that the signal 123 is provided to wire x.
   * x AND y -> z means that the bitwise AND of wire x and wire y is provided to
     wire z.
   * p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and
     then provided to wire q.
   * NOT e -> f means that the bitwise complement of the value from wire e is
     provided to wire f.

   Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for
   some reason, you'd like to emulate the circuit instead, almost all programming
   languages (for example, C, JavaScript, or Python) provide operators for these
   gates.

   For example, here is a simple circuit:

   123 -> x
   456 -> y
   x AND y -> d
   x OR y -> e
   x LSHIFT 2 -> f
   y RSHIFT 2 -> g
   NOT x -> h
   NOT y -> i
   After it is run, these are the signals on the wires:

   d: 72
   e: 507
   f: 492
   g: 114
   h: 65412
   i: 65079
   x: 123
   y: 456

   In little Bobby's kit's instructions booklet (provided as your puzzle input),
   what signal is ultimately provided to wire a?

   --- Part Two ---
   Now, take the signal you got on wire a, override wire b to that signal, and
   reset the other wires (including wire a). What new signal is ultimately provided
   to wire a?
-}


module D07_2015 exposing (Instruction(..), InstructionTable, VoltageTable, buildVoltageTable, eval1, eval2, getNum, main, parse, part1, part2, voltage, wireA)

import AdventOfCode exposing (Model, Msg, aoc, outFormat)
import Bitwise
import Dict exposing (Dict)


main : Program () Model Msg
main =
    aoc "data/d07_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 =
    wireA


part2 : List String -> Int
part2 instructions =
    let
        voltageTable =
            buildVoltageTable "a"
                (List.foldl parse Dict.empty instructions)
                (Dict.singleton "b" (wireA instructions))
    in
    Dict.get "a" voltageTable |> Maybe.withDefault -1


type Instruction
    = Assign String
    | And String String
    | Or String String
    | LShift String String
    | RShift String String
    | Not String


type alias VoltageTable =
    Dict String Int


type alias InstructionTable =
    Dict String Instruction


wireA : List String -> Int
wireA instructions =
    let
        voltageTable =
            buildVoltageTable "a" (List.foldl parse Dict.empty instructions) Dict.empty
    in
    Dict.get "a" voltageTable |> Maybe.withDefault -1


buildVoltageTable : String -> InstructionTable -> VoltageTable -> VoltageTable
buildVoltageTable wire instructions voltageTable =
    let
        newVoltageTable =
            Dict.foldl voltage voltageTable instructions
    in
    case Dict.get wire newVoltageTable of
        Just v ->
            newVoltageTable

        Nothing ->
            if Dict.size instructions == 0 then
                Dict.empty

            else
                buildVoltageTable wire instructions newVoltageTable


voltage : String -> Instruction -> VoltageTable -> VoltageTable
voltage wire instruction voltageTable =
    case instruction of
        Assign arg ->
            eval1 arg wire voltageTable (\a -> a)

        Not arg ->
            eval1 arg wire voltageTable (\a -> Bitwise.complement a |> Bitwise.and 65535)

        And arg1 arg2 ->
            eval2 arg1 arg2 wire voltageTable (\a -> \b -> Bitwise.and a b)

        Or arg1 arg2 ->
            eval2 arg1 arg2 wire voltageTable (\a -> \b -> Bitwise.or a b)

        LShift arg numBits ->
            eval2 arg numBits wire voltageTable (\a -> \b -> Bitwise.shiftLeftBy b a)

        RShift arg numBits ->
            eval2 arg numBits wire voltageTable (\a -> \b -> Bitwise.shiftRightBy b a)


eval1 : String -> String -> VoltageTable -> (Int -> Int) -> VoltageTable
eval1 arg wire voltageTable op =
    if Dict.member wire voltageTable then
        voltageTable

    else
        case getNum arg voltageTable of
            Just v ->
                Dict.insert wire (op v) voltageTable

            Nothing ->
                voltageTable


eval2 : String -> String -> String -> VoltageTable -> (Int -> Int -> Int) -> VoltageTable
eval2 arg1 arg2 wire voltageTable op =
    if Dict.member wire voltageTable then
        voltageTable

    else
        case ( getNum arg1 voltageTable, getNum arg2 voltageTable ) of
            ( Just num1, Just num2 ) ->
                Dict.insert wire (op num1 num2) voltageTable

            ( Just num1, Nothing ) ->
                Dict.insert arg1 num1 voltageTable

            ( Nothing, Just num2 ) ->
                Dict.insert arg2 num2 voltageTable

            ( Nothing, Nothing ) ->
                voltageTable


getNum : String -> VoltageTable -> Maybe Int
getNum arg voltageTable =
    case Dict.get arg voltageTable of
        Just v ->
            Just v

        Nothing ->
            String.toInt arg


parse : String -> InstructionTable -> InstructionTable
parse instructText =
    case String.words instructText of
        [ arg1, "AND", arg2, "->", wire ] ->
            Dict.insert wire (And arg1 arg2)

        [ arg1, "OR", arg2, "->", wire ] ->
            Dict.insert wire (Or arg1 arg2)

        [ arg1, "LSHIFT", numBits, "->", wire ] ->
            Dict.insert wire (LShift arg1 numBits)

        [ arg1, "RSHIFT", numBits, "->", wire ] ->
            Dict.insert wire (RShift arg1 numBits)

        [ "NOT", arg1, "->", wire ] ->
            Dict.insert wire (Not arg1)

        [ arg1, "->", wire ] ->
            Dict.insert wire (Assign arg1)

        _ ->
            identity
