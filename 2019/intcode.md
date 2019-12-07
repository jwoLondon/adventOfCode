---
follows: ../aoc.md

id: "litvis"
---

@import "../css/aoc.less"

# IntCode

_Collection of types and functions for representing an IntCode computer. Can be used with any puzzles that require it._

We need to be able to store the entire program, read values at given addresses and write to them. Although this could be stored in an array, for greater flexibility such as the use of non consecutive memory addresses, we will use a dictionary where the keys are addresses and the values the content at each address.

The computer also has some other states such as the input and output values used and produced by a program. To keep the design open to further enhancement, we can represent the state of the computer as a record (refactored from an earlier stage on where just the memory was stored).

## Computer Data Structure

```elm {l}
type alias Computer =
    { mem : Dict Int Int
    , outputStore : List Int
    , inputStore : List Int
    , startPointer : Int
    , log : List String
    , out : Int
    , status : Status
    }
```

The computer can be running a program, or paused while it awaits some external input (introduced on Day 7), or it may have halted after completing all instructions.

```elm {l}
type Status
    = Running
    | AwaitingInput
    | Halted
```

We can create a new computer by supplying optional inputs and a program.

```elm {l}
initComputer : List Int -> List Int -> Computer
initComputer inputs instrs =
    Computer (List.indexedMap Tuple.pair instrs |> Dict.fromList) [] inputs 0 [] -9999 Running
```

## Memory

How memory addresses are read depends on the _parameter mode_ (introduced on Day 5). _Position mode_ reads the value stored at a given address while _immediate mode_ reads a given value directly.

```elm {l}
type ParameterMode
    = Position
    | Immediate
```

```elm {l}
read : ParameterMode -> Int -> Computer -> Int
read md addr comp =
    case md of
        Immediate ->
            comp.mem
                |> Dict.get addr
                |> Maybe.withDefault -999

        Position ->
            read Immediate (Dict.get addr comp.mem |> Maybe.withDefault -999) comp
```

## Operations

The range of op codes representing instructions is stored as a custom type.

```elm {l}
type OpCode
    = Add Int Int
    | Mult Int Int
    | Input Int
    | Output Int
    | JmpIfTrue Int Int
    | JmpIfFalse Int Int
    | LessThan Int Int Int
    | Equals Int Int Int
    | Halt
    | NoOp
```

As the computer reads an instruction it finds the opcode and its parameters (if it has any) based in the integers read from memory. Which addresses these are read from will depend on the parameter mode, which itself is determined by the digits prior to the final two.

```elm {l}
readOp : Int -> Computer -> OpCode
readOp address comp =
    let
        toMode chr =
            case chr of
                '1' ->
                    Immediate

                _ ->
                    Position

        ( modes, opcode ) =
            ( read Immediate address comp // 100 |> String.fromInt |> String.toList |> List.map toMode |> List.reverse
            , modBy 100 (read Immediate address comp)
            )

        readParam =
            let
                md1 =
                    modes |> List.head |> Maybe.withDefault Position
            in
            read md1 (address + 1) comp

        read2Params =
            let
                md1 =
                    modes |> List.head |> Maybe.withDefault Position

                md2 =
                    modes |> List.drop 1 |> List.head |> Maybe.withDefault Position
            in
            ( read md1 (address + 1) comp, read md2 (address + 2) comp )
    in
    case opcode of
        1 ->
            let
                ( p1, p2 ) =
                    read2Params
            in
            Add p1 p2

        2 ->
            let
                ( p1, p2 ) =
                    read2Params
            in
            Mult p1 p2

        3 ->
            Input (read Immediate (address + 1) comp)

        4 ->
            Output readParam

        5 ->
            let
                ( p1, p2 ) =
                    read2Params
            in
            JmpIfTrue p1 p2

        6 ->
            let
                ( p1, p2 ) =
                    read2Params
            in
            JmpIfFalse p1 p2

        7 ->
            let
                ( p1, p2 ) =
                    read2Params

                p3 =
                    read Immediate (address + 3) comp
            in
            LessThan p1 p2 p3

        8 ->
            let
                ( p1, p2 ) =
                    read2Params

                p3 =
                    read Immediate (address + 3) comp
            in
            Equals p1 p2 p3

        99 ->
            Halt

        _ ->
            NoOp |> Debug.log "Unknown opcode read"
```

We can provide an equivalent function for applying the operation and if relevant, writing the result to memory:

```elm {l}
applyOp : OpCode -> Int -> Computer -> Computer
applyOp opCode address comp =
    case opCode of
        Add p1 p2 ->
            { comp | mem = Dict.insert (read Immediate (address + 3) comp) (p1 + p2) comp.mem }

        Mult p1 p2 ->
            { comp | mem = Dict.insert (read Immediate (address + 3) comp) (p1 * p2) comp.mem }

        Input p1 ->
            case comp.inputStore of
                inp :: tl ->
                    { comp
                        | mem = Dict.insert p1 inp comp.mem
                        , inputStore = tl
                        , status = Running
                    }

                _ ->
                    { comp | status = AwaitingInput }

        Output p1 ->
            { comp | outputStore = comp.outputStore ++ [ read Immediate (address + 1) comp ] }

        JmpIfTrue p1 p2 ->
            comp

        JmpIfFalse p1 p2 ->
            comp

        LessThan p1 p2 p3 ->
            let
                bool =
                    if p1 < p2 then
                        1

                    else
                        0
            in
            { comp | mem = Dict.insert p3 bool comp.mem }

        Equals p1 p2 p3 ->
            let
                bool =
                    if p1 == p2 then
                        1

                    else
                        0
            in
            { comp | mem = Dict.insert p3 bool comp.mem }

        Halt ->
            comp

        NoOp ->
            comp
```

We can run a program by starting at the opcode at position 0 and recursively processing opcodes until awaiting input or reading a halting opcode. To support debugging, we can also keep track in a log of mnemonics describing each operation (`&` indicates an address pointer).

```elm {l}
runProg : Computer -> Computer
runProg computer =
    let
        run address comp =
            if comp.status == Halted || (comp.status == AwaitingInput && comp.inputStore == []) then
                comp

            else
                let
                    op =
                        readOp address comp

                    addrStr addr =
                        "&" ++ String.fromInt addr ++ " "

                    numStr n =
                        String.fromInt n ++ " "
                in
                case op of
                    Add p1 p2 ->
                        let
                            log =
                                (addrStr address ++ ": Add " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr (address + 3) ++ "\n") :: comp.log
                        in
                        run (address + 4) (applyOp op address { comp | log = log })

                    Mult p1 p2 ->
                        let
                            log =
                                (addrStr address ++ ": Mult " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr (address + 3) ++ "\n") :: comp.log
                        in
                        run (address + 4) (applyOp op address { comp | log = log })

                    Input p1 ->
                        let
                            log =
                                case List.head comp.inputStore of
                                    Just inp ->
                                        (addrStr address ++ ": " ++ numStr inp ++ "→ Input → " ++ addrStr p1 ++ "\n") :: comp.log

                                    Nothing ->
                                        comp.log
                        in
                        run (address + 2) (applyOp op address { comp | log = log })

                    Output p1 ->
                        let
                            log =
                                (addrStr address ++ ": **OUT →** " ++ numStr p1 ++ "\n") :: comp.log
                        in
                        run (address + 2) (applyOp op address { comp | log = log, out = p1, startPointer = address + 2 })

                    JmpIfTrue p1 p2 ->
                        let
                            newAddress =
                                if p1 == 0 then
                                    address + 3

                                else
                                    p2

                            log =
                                (addrStr address ++ ": JmpTru " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr newAddress ++ "\n") :: comp.log
                        in
                        run newAddress (applyOp op newAddress { comp | log = log })

                    JmpIfFalse p1 p2 ->
                        let
                            newAddress =
                                if p1 == 0 then
                                    p2

                                else
                                    address + 3

                            log =
                                (addrStr address ++ ": JmpFls " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr newAddress ++ "\n") :: comp.log
                        in
                        run newAddress (applyOp op newAddress { comp | log = log })

                    LessThan p1 p2 p3 ->
                        let
                            log =
                                (addrStr address ++ ": Less " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr p3 ++ "\n") :: comp.log
                        in
                        run (address + 4) (applyOp op address { comp | log = log })

                    Equals p1 p2 p3 ->
                        let
                            log =
                                (addrStr address ++ ": Equal " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr p3 ++ "\n") :: comp.log
                        in
                        run (address + 4) (applyOp op address { comp | log = log })

                    Halt ->
                        { comp
                            | status = Halted
                            , log = (addrStr address ++ ": HALT\n") :: comp.log |> List.reverse
                        }

                    NoOp ->
                        { comp | log = (addrStr address ++ ": **Bad Exit**\n") :: comp.log |> List.reverse }
    in
    run computer.startPointer computer
```

Execution will pause on input if there are no values in the input queue. New input values can be added to the queue with `addInput`. This doesn't automatically start a paused execution, but a subsequent call to `runProg`.

```elm {l}
addInput : Int -> Computer -> Computer
addInput input comp =
    { comp | inputStore = input :: comp.inputStore }
```

### Examples

First example from [day 2](d02_2019.md), should generate output of 3500 after setting the 'noun' (address 1) to 9 and 'verb' (address 2) to 10 within the program instructions.

```elm {l r}
test1 : Int
test1 =
    let
        comp =
            [ 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 ]
                |> initComputer [ 0 ]
    in
    { comp
        | mem =
            comp.mem
                |> Dict.insert 1 9
                |> Dict.insert 2 10
    }
        |> runProg
        |> read Immediate 0
```

This should place 123 in input, store it at address 50 and then send the value at that address to output.

```elm {l m}
test2 : List String
test2 =
    [ 3, 50, 4, 50, 99 ]
        |> initComputer [ 123 ]
        |> runProg
        |> .log
```

Multiply value at address 4 (33) by the immediate value 3 and place result (99) at address 4 and therefore halt.

```elm {l m}
test3 : List String
test3 =
    [ 1002, 4, 3, 4, 33 ]
        |> initComputer [ 0 ]
        |> runProg
        |> .log
```

If input is equal to 8, output a 1, otherwise output a 0

```elm {l m}
test4 : List String
test4 =
    [ 3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8 ]
        |> initComputer [ 8 ]
        |> runProg
        |> .log
```

If input is less than 8, output a 1, otherwise output a 0

```elm {l m}
test5 : List String
test5 =
    [ 3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8 ]
        |> initComputer [ 8 ]
        |> runProg
        |> .log
```

If the input is less than 8, output should be 999, if equal to 9 it should be 1000 and if greater than 8, it should be 1001.

```elm {l m}
test6 : List String
test6 =
    [ 3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99 ]
        |> initComputer [ 9 ]
        |> runProg
        |> .log
```

Should output the first input (13) and then pause awaiting second input. Note to reverse the log list when in a paused state.

```elm {l m}
test7 : List String
test7 =
    [ 3, 10, 4, 10, 3, 11, 4, 11, 99 ]
        |> initComputer [ 13 ]
        |> runProg
        |> .log
        |> List.reverse
```

As with the previous test, but should resume after receiving a second input (25) before halting.

```elm {l m}
test8 : List String
test8 =
    [ 3, 10, 4, 10, 3, 11, 4, 11, 99 ]
        |> initComputer [ 13 ]
        |> runProg
        |> addInput 25
        |> runProg
        |> .log
```
