{- Little Jane Marie just got her very first computer for Christmas from some unknown
   benefactor. It comes with instructions and an example program, but the computer
   itself seems to be malfunctioning. She's curious what the program does, and would
   like you to help her run it.

   The manual explains that the computer supports two registers and six instructions
   (truly, it goes on to remind the reader, a state-of-the-art technology). The
   registers are named a and b, can hold any non-negative integer, and begin with
   a value of 0. The instructions are as follows:

   hlf r sets register r to half its current value, then continues with the next instruction.
   tpl r sets register r to triple its current value, then continues with the next instruction.
   inc r increments register r, adding 1 to it, then continues with the next instruction.
   jmp offset is a jump; it continues with the instruction offset away relative to itself.
   jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
   jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).

   All three jump instructions work with an offset relative to that instruction.
   The offset is always written with a prefix + or - to indicate the direction of
   the jump (forward or backward, respectively). For example, jmp +1 would simply
   continue with the next instruction, while jmp +0 would continuously jump back
   to itself forever.

   The program exits when it tries to run an instruction beyond the ones defined.

   For example, this program sets a to 2, because the jio instruction causes it
   to skip the tpl instruction:

   inc a
   jio a, +2
   tpl a
   inc a

   What is the value in register b when the program in your puzzle input is finished
   executing?

   --- Part Two ---

   The unknown benefactor is very thankful for releasi-- er, helping little Jane
   Marie with her computer. Definitely not to distract you, what is the value in
   register b after the program is finished executing if register a starts as 1
   instead?

-}


module Main exposing (..)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, toInt)
import Array exposing (Array)
import Dict exposing (Dict)
import Regex exposing (Regex)


type alias Registers =
    Dict Char Int


type Instruction
    = Hlf Char
    | Tpl Char
    | Inc Char
    | Jmp Int
    | Jie Char Int
    | Jio Char Int


main : Program Never Model Msg
main =
    aoc "data/d23_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 instructions =
    parse instructions
        |> run 0 (Dict.fromList [ ( 'a', 0 ), ( 'b', 0 ) ])
        |> get 'b'


part2 : List String -> Int
part2 instructions =
    parse instructions
        |> run 0 (Dict.fromList [ ( 'a', 1 ), ( 'b', 0 ) ])
        |> get 'b'


get : Char -> Registers -> Int
get regName registers =
    Dict.get regName registers |> Maybe.withDefault 0


run : Int -> Registers -> Array Instruction -> Registers
run pos registers program =
    case Array.get pos program of
        Just (Hlf reg) ->
            run (pos + 1) (Dict.insert reg (get reg registers // 2) registers) program

        Just (Tpl reg) ->
            run (pos + 1) (Dict.insert reg (get reg registers * 3) registers) program

        Just (Inc reg) ->
            run (pos + 1) (Dict.insert reg (get reg registers + 1) registers) program

        Just (Jmp offset) ->
            run (pos + offset) registers program

        Just (Jie reg offset) ->
            if get reg registers % 2 == 0 then
                run (pos + offset) registers program
            else
                run (pos + 1) registers program

        Just (Jio reg offset) ->
            if get reg registers == 1 then
                run (pos + offset) registers program
            else
                run (pos + 1) registers program

        Nothing ->
            registers


parse : List String -> Array Instruction
parse =
    List.foldl parseLine [] >> List.reverse >> Array.fromList


parseLine : String -> List Instruction -> List Instruction
parseLine text instructions =
    let
        matches text =
            text
                |> Regex.find (Regex.AtMost 1)
                    (Regex.regex "(\\w+) ([ab]|[+-]\\d+)(?:, )?([+-]\\d+)?")
                |> List.map .submatches

        toChar =
            String.toList >> List.head >> Maybe.withDefault 'X'
    in
    case matches text of
        [ [ Just "hlf", Just reg, Nothing ] ] ->
            Hlf (toChar reg) :: instructions

        [ [ Just "tpl", Just reg, Nothing ] ] ->
            Tpl (toChar reg) :: instructions

        [ [ Just "inc", Just reg, Nothing ] ] ->
            Inc (toChar reg) :: instructions

        [ [ Just "jmp", Just offset, Nothing ] ] ->
            Jmp (toInt offset) :: instructions

        [ [ Just "jie", Just reg, Just offset ] ] ->
            Jie (toChar reg) (toInt offset) :: instructions

        [ [ Just "jio", Just reg, Just offset ] ] ->
            Jio (toChar reg) (toInt offset) :: instructions

        _ ->
            instructions
