{- You decide to head directly to the CPU and fix the printer from there. As you
   get close, you find an experimental coprocessor doing so much work that the local
   programs are afraid it will halt and catch fire. This would cause serious issues
   for the rest of the computer, so you head in and see what you can do.

   The code it's running seems to be a variant of the kind you saw recently on that
   tablet. The general functionality seems very similar, but some of the instructions
   are different:

   * set X Y sets register X to the value of Y.
   * sub X Y decreases register X by the value of Y.
   * mul X Y sets register X to the result of multiplying the value contained in
     register X by the value of Y.
   * jnz X Y jumps with an offset of the value of Y, but only if the value of X
     is not zero. (An offset of 2 skips the next instruction, an offset of -1 jumps
     to the previous instruction, and so on.)

   Only the instructions listed above are used. The eight registers here, named a
   through h, all start at 0.

   The coprocessor is currently set to some kind of debug mode, which allows for
   testing, but prevents it from doing any meaningful work.

   If you run the program (your puzzle input), how many times is the mul instruction
   invoked?

   --- Part Two ---

   Now, it's time to fix the problem.

   The debug mode switch is wired directly to register a. You flip the switch,
   which makes register a now start at 1 when the program is executed.

   Immediately, the coprocessor begins to overheat. Whoever wrote this program
   obviously didn't choose a very efficient implementation. You'll need to optimize
   the program if it has any hope of completing before Santa needs that printer working.

   The coprocessor's ultimate goal is to determine the final value left in register
   h once the program completes. Technically, if it had that... it wouldn't even need
   to run the program.

   After setting register a to 1, if the program were to run to completion, what
   value would be left in register h?
-}


module D23_2017 exposing (Expression(..), Instruction(..), Registers, eval, get, isComposite, isPrime, main, parse, parseLine, part1, part2, registers, run)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat, submatches, toInt)
import Array exposing (Array)
import Dict exposing (Dict)


main : Program () Model Msg
main =
    aoc "../data/d23_2017.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


type alias Registers =
    Dict Char Int


type Expression
    = Literal Int
    | Register Char


type Instruction
    = Set Char Expression
    | Sub Char Expression
    | Mul Char Expression
    | Jnz Expression Expression


registers : Registers
registers =
    -- Use extra register z to store number of multiplications.
    "abcdefghz"
        |> String.toList
        |> List.map (\r -> ( r, 0 ))
        |> Dict.fromList


part1 : List String -> Int
part1 =
    parse
        >> run 0 registers
        >> get 'z'



{- For Part 2, the input assembler code counts all the composite numbers (non prime)
   between the a lower and upper bounds determined by registers b and c in increments
   of 17. The first 8 instructions determine the bounds and the penulimate instruction
   the increment.

   set b 67            -- INIT_b = 67
   set c b

   jnz a 2             -- In Part 1 (a==0) b and c are equal
   jnz 1 5
   mul b 100
   sub b -100000       -- b = INIT_b * 100 + 100000
   set c b
   sub c -17000        -- c = b + 17000


     set f 1           -- outer loop start
     set d 2           -- middle loop start
       set e 2         -- inner loop start
         set g d
         mul g e
         sub g b
         jnz g 2
         set f 0       -- Set f to 0 if d has a factor
         sub e -1
         set g e
         sub g b
       jnz g -8        -- } end of inner for loop

       sub d -1
       set g d
       sub g b
     jnz g -13         -- } end of middle loop

     jnz f 2
     sub h -1          -- Incement h if any factors (i.e. not prime)
     set g b
     sub g c
     jnz g 2           -- HALT if reached upper limit (c)
     jnz 1 3
     sub b -17
     jnz 1 -23         -- } end of outer loop
-}


part2 : List String -> Int
part2 input =
    let
        initRegs =
            input
                |> parse
                |> Array.slice 0 8
                |> run 0 (registers |> Dict.insert 'a' 1)

        lower =
            get 'b' initRegs

        upper =
            get 'c' initRegs

        inc =
            input
                |> List.drop (List.length input - 2)
                |> List.head
                |> Maybe.withDefault ""
                |> String.dropLeft 6
                |> toInt
                |> (*) -1
    in
    List.range 0 ((upper - lower) // inc)
        |> List.filter (\n -> isComposite (n * inc + lower))
        |> List.length


run : Int -> Registers -> Array Instruction -> Registers
run pos regs program =
    case Array.get pos program of
        Just (Set r expr) ->
            run (pos + 1) (Dict.insert r (eval expr regs) regs) program

        Just (Sub r expr) ->
            run (pos + 1) (Dict.insert r (get r regs - eval expr regs) regs) program

        Just (Mul r expr) ->
            let
                reg2 =
                    Dict.insert 'z' (get 'z' regs + 1) regs
            in
            run (pos + 1) (Dict.insert r (get r reg2 * eval expr reg2) reg2) program

        Just (Jnz expr1 expr2) ->
            if eval expr1 regs /= 0 then
                run (pos + eval expr2 regs) regs program

            else
                run (pos + 1) regs program

        Nothing ->
            regs


isComposite : Int -> Bool
isComposite =
    not << isPrime


isPrime : Int -> Bool
isPrime n =
    if n < 2 then
        False

    else
        let
            candidates =
                List.range 2 (floor (sqrt (toFloat n)))
        in
        List.length (List.filter (\fac -> modBy fac n == 0) candidates) == 0


get : Char -> Registers -> Int
get regName regs =
    Dict.get regName regs |> Maybe.withDefault 0


eval : Expression -> Registers -> Int
eval expr regs =
    case expr of
        Literal i ->
            i

        Register reg ->
            get reg regs


parse : List String -> Array Instruction
parse =
    List.foldl parseLine []
        >> List.reverse
        >> Array.fromList


parseLine : String -> List Instruction -> List Instruction
parseLine text instructions =
    let
        toExpression regOrNum =
            case String.toInt regOrNum of
                Just num ->
                    Literal num

                Nothing ->
                    Register (toChar regOrNum)

        toChar s =
            String.toList s |> List.head |> Maybe.withDefault 'X'
    in
    case submatches "(\\w+) ([a-h]|[-]?\\d+) ?([a-h]|[-]?\\d+)?" text of
        [ Just "set", Just reg, Just regOrNum2 ] ->
            Set (toChar reg) (toExpression regOrNum2) :: instructions

        [ Just "sub", Just reg, Just regOrNum2 ] ->
            Sub (toChar reg) (toExpression regOrNum2) :: instructions

        [ Just "mul", Just reg, Just regOrNum2 ] ->
            Mul (toChar reg) (toExpression regOrNum2) :: instructions

        [ Just "jnz", Just regOrNum1, Just regOrNum2 ] ->
            Jnz (toExpression regOrNum1) (toExpression regOrNum2) :: instructions

        _ ->
            instructions |> Debug.log "Unknown instruction"
