{- You receive a signal directly from the CPU. Because of your recent assistance
   with jump instructions, it would like you to compute the result of a series of
   unusual register instructions.

   Each instruction consists of several parts: the register to modify, whether to
   increase or decrease that register's value, the amount by which to increase or
   decrease it, and a condition. If the condition fails, skip the instruction
   without modifying the register. The registers all start at 0.
   The instructions look like this:

   b inc 5 if a > 1
   a inc 1 if b < 5
   c dec -10 if a >= 1
   c inc -20 if c == 10

   These instructions would be processed as follows:

   Because a starts at 0, it is not greater than 1, and so b is not modified.
   a is increased by 1 (to 1) because b is less than 5 (it is 0).
   c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
   c is increased by -20 (to -10) because c is equal to 10.

   After this process, the largest value in any register is 1.

   You might also encounter <= (less than or equal to) or != (not equal to).
   However, the CPU doesn't have the bandwidth to tell you what all the registers
   are named, and leaves that to you to determine.

   What is the largest value in any register after completing the instructions
   in your puzzle input?

   --- Part Two ---

   To be safe, the CPU also needs to know the highest value held in any register
   during this process so that it can decide how much memory to allocate to these
   operations. For example, in the above instructions, the highest value ever held
   was 10 (in register c after the third instruction was evaluated).
-}


module D08_2017 exposing (..)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, toInt)
import Dict exposing (Dict)
import Regex exposing (regex)


type alias Operation =
    { reg : String
    , delta : Int
    , cReg : String
    , cOp : Int -> Int -> Bool
    , cVal : Int
    }


type alias Registers =
    Dict String Int


maxReg : String
maxReg =
    "MAX_REG"


main : Program Never Model Msg
main =
    aoc "data/d08_2017.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 =
    List.map parseLine
        >> List.foldl apply Dict.empty
        >> Dict.remove maxReg
        >> Dict.values
        >> List.maximum
        >> Maybe.withDefault 0


part2 : List String -> Int
part2 =
    List.map parseLine
        >> List.foldl apply Dict.empty
        >> getReg maxReg


apply : Operation -> Registers -> Registers
apply op registers =
    if op.cOp (getReg op.cReg registers) op.cVal then
        let
            newVal =
                getReg op.reg registers + op.delta
        in
        Dict.insert maxReg (max newVal (getReg maxReg registers)) registers
            |> Dict.insert op.reg newVal
    else
        registers


getReg : String -> Registers -> Int
getReg reg registers =
    case Dict.get reg registers of
        Just val ->
            val

        Nothing ->
            0


parseLine : String -> Operation
parseLine =
    let
        toOperation : List String -> Operation
        toOperation tokens =
            case tokens of
                reg :: deltaOp :: delta :: cReg :: compOp :: compVal :: [] ->
                    let
                        deltaVal =
                            if deltaOp == "inc" then
                                toInt delta
                            else
                                -1 * toInt delta

                        cOp =
                            case compOp of
                                "==" ->
                                    (==)

                                "!=" ->
                                    (/=)

                                "<" ->
                                    (<)

                                ">" ->
                                    (>)

                                "<=" ->
                                    (<=)

                                ">=" ->
                                    (>=)

                                _ ->
                                    (==) |> Debug.log "Bad comparison op"
                    in
                    Operation reg deltaVal cReg cOp (toInt compVal)

                _ ->
                    Operation "" 0 "" (==) 0 |> Debug.log "Bad input"
    in
    Regex.find Regex.All (Regex.regex "(\\w+) (inc|dec) (-?\\d+) if (\\w+) ([<>=!]+) (-?\\d+)")
        >> List.concatMap .submatches
        >> List.filterMap identity
        >> toOperation
