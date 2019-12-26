---
follows: ../aoc.md

id: "litvis"
---

@import "../css/aoc.less"

# IntCode

_Collection of types and functions for representing an IntCode computer. Can be used with any puzzles that require it._

We need to be able to store an intcode program, read values at given addresses and write to them. We also need to be able to receive input and generate output.

## Data Structure

```elm {l}
type alias Computer =
    { mem : Dict Int Int
    , outputStore : List Int
    , inputStore : List Int
    , startPointer : Int
    , relativeBase : Int
    , log : BoundedDeque String
    , out : Int
    , status : Status
    }
```

## Initialising

We can create a new computer by supplying optional inputs and a program.

```elm {l}
initComputer : List Int -> List Int -> Computer
initComputer inputs instrs =
    Computer (List.indexedMap Tuple.pair instrs |> Dict.fromList) [] inputs 0 0 (BoundedDeque.empty 100) -9999 Running
```

The computer can be running a program, or paused while it awaits some external input, or it may have halted after completing all instructions.

```elm {l}
type Status
    = Running
    | AwaitingInput
    | Halted
```

## Debugging

For debugging, running the computer generates a log of instructions as it runs. To view the log:

```elm {l}
commandLog : Computer -> List String
commandLog comp =
    BoundedDeque.toList comp.log
```

To preserve resources, this log has a maximum buffer size. To change from the default buffer size of 100 instructions:

```elm {l}
resizeLogBuffer : Int -> Computer -> Computer
resizeLogBuffer bSize computer =
    let
        logBuffer =
            computer.log
                |> BoundedDeque.toList
                |> List.reverse
                |> List.take bSize
                |> List.reverse
    in
    { computer | log = BoundedDeque.fromList bSize logBuffer }
```

## Memory

How memory addresses are read depends on the _parameter mode_ (introduced on Day 5). _Immediate mode_ reads a given value directly while _Position mode_ reads a value stored at a given address. _Relative mode_ also reads from addresses, but offset by whatever the current _relativeBase_ is (changed with the `ShiftBase` (9) command).

```elm {l}
type ParameterMode
    = Position
    | Immediate
    | Relative
```

```elm {l}
read : ParameterMode -> Int -> Computer -> Int
read md addr comp =
    case md of
        Immediate ->
            comp.mem
                |> Dict.get addr
                |> Maybe.withDefault 0

        Position ->
            read Immediate
                (Dict.get addr comp.mem |> Maybe.withDefault 0)
                comp

        Relative ->
            read Immediate
                (comp.relativeBase
                    + (Dict.get addr comp.mem |> Maybe.withDefault 0)
                )
                comp
```

To manipulate memory contents directly we can poke a value at a given address:

```elm {l}
poke : Int -> Int -> Computer -> Computer
poke addr val comp =
    { comp | mem = comp.mem |> Dict.insert addr val }
```

## Operations

The range of op codes representing instructions is stored as a custom type.

```elm {l}
type OpCode
    = Add Int Int Int
    | Mult Int Int Int
    | Input Int
    | Output Int
    | JmpIfTrue Int Int
    | JmpIfFalse Int Int
    | LessThan Int Int Int
    | Equals Int Int Int
    | ShiftBase Int
    | Halt
    | NoOp
```

As the computer reads an instruction it finds the opcode and its parameters (if it has any) based in the values read from memory. Which addresses these are read from will depend on the _parameter mode_, which itself is determined by the digits prior to the final two.

```elm {l=hidden}
readOp : Int -> Computer -> OpCode
readOp address comp =
    let
        toMode chr =
            case chr of
                '1' ->
                    Immediate

                '2' ->
                    Relative

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

        readAddressParam a =
            let
                md1 =
                    modes |> List.head |> Maybe.withDefault Position

                offset =
                    if md1 == Relative then
                        comp.relativeBase

                    else
                        0
            in
            offset + read Immediate (address + a) comp

        readAddressModeParam md a =
            let
                offset =
                    if md == Relative then
                        comp.relativeBase

                    else
                        0
            in
            offset + read Immediate (address + a) comp

        read3Params =
            let
                md1 =
                    modes |> List.head |> Maybe.withDefault Position

                md2 =
                    modes |> List.drop 1 |> List.head |> Maybe.withDefault Position

                md3 =
                    modes |> List.drop 2 |> List.head |> Maybe.withDefault Position
            in
            ( read md1 (address + 1) comp, read md2 (address + 2) comp, readAddressModeParam md3 3 )
    in
    case opcode of
        1 ->
            let
                ( p1, p2, p3 ) =
                    read3Params
            in
            Add p1 p2 p3

        2 ->
            let
                ( p1, p2, p3 ) =
                    read3Params
            in
            Mult p1 p2 p3

        3 ->
            Input (readAddressParam 1)

        4 ->
            Output readParam

        5 ->
            let
                ( p1, p2, p3 ) =
                    read3Params
            in
            JmpIfTrue p1 p2

        6 ->
            let
                ( p1, p2, p3 ) =
                    read3Params
            in
            JmpIfFalse p1 p2

        7 ->
            let
                ( p1, p2, p3 ) =
                    read3Params
            in
            LessThan p1 p2 p3

        8 ->
            let
                ( p1, p2, p3 ) =
                    read3Params
            in
            Equals p1 p2 p3

        9 ->
            ShiftBase readParam

        99 ->
            Halt

        _ ->
            NoOp |> Debug.log "Unknown opcode read"
```

We can provide an equivalent function for applying the operation and if relevant, writing the result to memory.

```elm {l=hidden}
applyOp : OpCode -> Int -> Computer -> Computer
applyOp opCode address comp =
    case opCode of
        Add p1 p2 p3 ->
            { comp | mem = Dict.insert p3 (p1 + p2) comp.mem }

        Mult p1 p2 p3 ->
            { comp | mem = Dict.insert p3 (p1 * p2) comp.mem }

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
            comp

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

        ShiftBase p1 ->
            { comp | relativeBase = comp.relativeBase + p1 }

        Halt ->
            comp

        NoOp ->
            comp
```

We can run a program by starting at the opcode at position 0 and recursively processing opcodes until awaiting input or reading a halting opcode.

```elm {l=hidden}
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
                    Add p1 p2 p3 ->
                        let
                            log =
                                BoundedDeque.pushBack (addrStr address ++ ": Add " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr p3 ++ "\n") comp.log
                        in
                        run (address + 4) (applyOp op address { comp | log = log })

                    Mult p1 p2 p3 ->
                        let
                            log =
                                BoundedDeque.pushBack (addrStr address ++ ": Mult " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr p3 ++ "\n") comp.log
                        in
                        run (address + 4) (applyOp op address { comp | log = log })

                    Input p1 ->
                        let
                            log =
                                case List.head comp.inputStore of
                                    Just inp ->
                                        BoundedDeque.pushBack (addrStr address ++ ": " ++ numStr inp ++ "→ Input → " ++ addrStr p1 ++ "\n") comp.log

                                    Nothing ->
                                        comp.log
                        in
                        run (address + 2) (applyOp op address { comp | log = log })

                    Output p1 ->
                        let
                            log =
                                BoundedDeque.pushBack (addrStr address ++ ": **OUT →** " ++ numStr p1 ++ "\n") comp.log
                        in
                        run (address + 2)
                            (applyOp op
                                address
                                { comp
                                    | log = log
                                    , out = p1
                                    , outputStore = comp.outputStore ++ [ p1 ]
                                    , startPointer = address + 2
                                }
                            )

                    JmpIfTrue p1 p2 ->
                        let
                            newAddress =
                                if p1 == 0 then
                                    address + 3

                                else
                                    p2

                            log =
                                BoundedDeque.pushBack (addrStr address ++ ": JmpTru " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr newAddress ++ "\n") comp.log
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
                                BoundedDeque.pushBack (addrStr address ++ ": JmpFls " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr newAddress ++ "\n") comp.log
                        in
                        run newAddress (applyOp op newAddress { comp | log = log })

                    LessThan p1 p2 p3 ->
                        let
                            log =
                                BoundedDeque.pushBack (addrStr address ++ ": Less " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr (address + 3) ++ "\n") comp.log
                        in
                        run (address + 4) (applyOp op address { comp | log = log })

                    Equals p1 p2 p3 ->
                        let
                            log =
                                BoundedDeque.pushBack (addrStr address ++ ": Equal " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr (address + 3) ++ "\n") comp.log
                        in
                        run (address + 4) (applyOp op address { comp | log = log })

                    ShiftBase p1 ->
                        let
                            log =
                                BoundedDeque.pushBack (addrStr address ++ ": ShiftBase " ++ numStr p1 ++ "\n") comp.log
                        in
                        run (address + 2) (applyOp op address { comp | log = log, startPointer = address + 2 })

                    Halt ->
                        { comp
                            | status = Halted
                            , log = BoundedDeque.pushBack (addrStr address ++ ": HALT\n") comp.log
                        }

                    NoOp ->
                        { comp | log = BoundedDeque.pushBack (addrStr address ++ ": **Bad Exit**\n") comp.log }
    in
    run computer.startPointer computer
```

Execution will pause on input if there are no values in the input queue. New input values can be added to the queue with `addInput` or in batches via `addInputs`. This doesn't automatically start a paused execution, requiring a further call to `runProg` to resume.

```elm {l}
addInput : Int -> Computer -> Computer
addInput input comp =
    { comp | inputStore = input :: comp.inputStore }


addInputs : List Int -> Computer -> Computer
addInputs inputs comp =
    { comp | inputStore = List.reverse inputs ++ comp.inputStore }
```

The state of the computer includes a list of its output instructions. Sometimes we may need to clear it.

```elm {l}
clearOutput : Computer -> Computer
clearOutput comp =
    { comp | outputStore = [] }
```

---

## Tests

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
        |> commandLog
```

Multiply value at address 4 (33) by the immediate value 3 and place result (99) at address 4 and therefore halt.

```elm {l m}
test3 : List String
test3 =
    [ 1002, 4, 3, 4, 33 ]
        |> initComputer [ 0 ]
        |> runProg
        |> commandLog
```

If input is equal to 8, output a 1, otherwise output a 0

```elm {l m}
test4 : List String
test4 =
    [ 3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8 ]
        |> initComputer [ 8 ]
        |> runProg
        |> commandLog
```

If input is less than 8, output a 1, otherwise output a 0

```elm {l m}
test5 : List String
test5 =
    [ 3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8 ]
        |> initComputer [ 8 ]
        |> runProg
        |> commandLog
```

If the input is less than 8, output should be 999, if equal to 9 it should be 1000 and if greater than 8, it should be 1001.

```elm {l m}
test6 : List String
test6 =
    [ 3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99 ]
        |> initComputer [ 9 ]
        |> runProg
        |> commandLog
```

Should output the first input (13) and then pause awaiting second input. Note to reverse the log list when in a paused state.

```elm {l m}
test7 : List String
test7 =
    [ 3, 10, 4, 10, 3, 11, 4, 11, 99 ]
        |> initComputer [ 13 ]
        |> runProg
        |> commandLog
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
        |> commandLog
```

Set the relative base to 50, make all operations relative and output the input value.

```elm {l m}
test9 : List String
test9 =
    [ 109, 50, 203, 0, 204, 0, 99 ]
        |> initComputer [ 13 ]
        |> runProg
        |> commandLog
```

Set the relative base to 50, make all operations relative and apply the test 5 (if input is less than 8, output 1, else output 0)

```elm {l m}
test10 : List String
test10 =
    [ 109, 50, 203, 11, 207, 11, 12, 11, 204, 11, 99, -1, 8 ]
        |> initComputer [ 5 ]
        |> runProg
        |> commandLog
```

Shift base to 500, then output the input value using relative mode..

```elm {l m}
test11 : List String
test11 =
    [ 109, 500, 203, 50, 204, 50, 99 ]
        |> initComputer [ 123 ]
        |> runProg
        |> commandLog
```

Should produce a copy of itself as output:

```elm {l m}
test12 : List String
test12 =
    [ 109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99 ]
        |> initComputer []
        |> runProg
        |> commandLog
        |> List.filter (\l -> String.contains "OUT" l)
```

Should output a 16 digit number.

```elm {l m}
test13 : List String
test13 =
    [ 1102, 34915192, 34915192, 7, 4, 7, 99, 0 ]
        |> initComputer []
        |> runProg
        |> commandLog
```

Should output the large number in the middle

```elm {l m}
test14 : List String
test14 =
    [ 104, 1125899906842624, 99 ]
        |> initComputer []
        |> runProg
        |> commandLog
```

### Relative Mode Tests

#### Add

```elm {l m}
rmAdd : List String
rmAdd =
    [ 109, 500, 203, 50, 2201, 50, 50, 60, 4, 60, 99 ]
        |> initComputer [ 13 ]
        |> runProg
        |> commandLog
```

#### Multiply

```elm {l m}
rmMult : List String
rmMult =
    [ 109, 500, 203, 50, 2202, 50, 50, 60, 4, 60, 99 ]
        |> initComputer [ 13 ]
        |> runProg
        |> commandLog
```

#### Jump If True

```elm {l m}
rmJit : List String
rmJit =
    [ 109, 500, 203, 50, 1205, 50, 10, 104, -1, 99, 104, 1, 99 ]
        |> initComputer [ 0 ]
        |> runProg
        |> commandLog
```

#### Jump If False

```elm {l m}
rmJif : List String
rmJif =
    [ 109, 500, 203, 50, 1206, 50, 10, 104, 1, 99, 104, -1, 99 ]
        |> initComputer [ 0 ]
        |> runProg
        |> commandLog
```

#### Less than

Is input negative?

```elm {l m}
rmLess : List String
rmLess =
    [ 109, 500, 203, 50, 1207, 50, 0, 7, 4, 7, 99 ]
        |> initComputer [ 1 ]
        |> runProg
        |> commandLog
```

Is is 77 less than 88 (always outputs a 1)?

```elm {l m}
rmLess2 : List String
rmLess2 =
    [ 109, 500, 21107, 77, 88, 50, 204, 50, 99 ]
        |> initComputer []
        |> runProg
        |> commandLog
```

#### Equal to

Is input 0?

```elm {l m}
rmEqual : List String
rmEqual =
    [ 109, 500, 203, 50, 1208, 50, 0, 7, 4, 7, 99 ]
        |> initComputer [ 0 ]
        |> runProg
        |> commandLog
```

#### Shift Base

Should show input three times: 1 in position mode, 2, after in relative mode and 3 in immediate mode

```elm {l m}
rmShiftBase : List String
rmShiftBase =
    [ 103, 50, 9, 50, 4, 50, 209, -450, 204, -950, 109, -1000, 204, 50, 99 ]
        |> initComputer [ 500 ]
        |> runProg
        |> commandLog
```
