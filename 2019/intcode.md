---
follows: ../aoc.md

id: "litvis"
---

@import "../css/aoc.less"

# IntCode

_Collection of types and functions for representing IntCode. Can be used with any puzzles that require it._

We need to be able to store the entire program, read values at given addresses and write to them. Although this could be stored in an array, for greater flexibility such as the use of non consecutive memory addresses, we will use a dictionary where the keys are addresses and the values the content at each address.

## Memory

```elm {l}
type alias Computer =
    { mem : Dict Int Int
    , outputStore : List Int
    , inputStore : Int
    }


initComputer : Int -> List Int -> Computer
initComputer inp instrs =
    Computer (List.indexedMap Tuple.pair instrs |> Dict.fromList) [] inp
```

Because we will be doing a lot of reading from the dictionary we can create an 'unsafe' dictionary reader to avoid littering code with handling `Maybe`. At least for the moment, given the only operations are addition and multiplication, we shouldn't need to handle negative numbers, so we can reserve these for possible errors.

```elm {l}
read : Int -> Computer -> Int
read address comp =
    comp.mem |> Dict.get address |> Maybe.withDefault -1
```

To aid program clarity, and to account for the possibility that further op codes may be added later, we can store each op code as a custom type:

```elm {l}
type OpCode
    = Add Int Int
    | Mult Int Int
    | Input Int
    | Output Int
    | Halt


readOp : Int -> Computer -> OpCode
readOp address comp =
    case read address comp of
        1 ->
            Add (read (read (address + 1) comp) comp) (read (read (address + 2) comp) comp)

        2 ->
            Mult (read (read (address + 1) comp) comp) (read (read (address + 2) comp) comp)

        3 ->
            Input (read (address + 1) comp)

        4 ->
            Output (read (read (address + 1) comp) comp)

        99 ->
            Halt

        _ ->
            Halt |> Debug.log "Unknown opcode read"
```

We can provide an equivalent function for applying the operation and doing the writing

```elm {l}
writeOp : OpCode -> Int -> Computer -> Computer
writeOp opCode address comp =
    case opCode of
        Halt ->
            comp

        Add p1 p2 ->
            { comp | mem = Dict.insert (read (address + 3) comp) (p1 + p2) comp.mem }

        Mult p1 p2 ->
            { comp | mem = Dict.insert (read (address + 3) comp) (p1 * p2) comp.mem }

        Input p1 ->
            { comp | mem = Dict.insert p1 comp.inputStore comp.mem }

        Output p1 ->
            { comp | outputStore = comp.outputStore ++ [ read (address + 1) comp ] }
```

We can run a program by starting at the opcode at position 0 and after halting, reading the (possibly new) value at address 0.

```elm {l}
runProg : Int -> Int -> Computer -> Int
runProg noun verb comp =
    let
        run address mem =
            let
                op =
                    readOp address mem
            in
            case op of
                Halt ->
                    mem

                _ ->
                    run (address + 4) (writeOp op address mem)
    in
    { comp
        | mem =
            comp.mem
                |> Dict.insert 1 noun
                |> Dict.insert 2 verb
    }
        |> run 0
        |> read 0
```

To support debugging, we can create a disassembler that shows the memory in a more readable fashion:

```elm {l}
debug : Computer -> List String
debug =
    let
        run address output comp =
            let
                op =
                    readOp address comp

                addrStr addr =
                    "|" ++ String.fromInt addr ++ "| "

                numStr n =
                    String.fromInt n ++ " "
            in
            case op of
                Add p1 p2 ->
                    run (address + 4)
                        ((addrStr address ++ "Add " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr (address + 3) ++ "\n") :: output)
                        (writeOp op address comp)

                Mult p1 p2 ->
                    run (address + 4)
                        ((addrStr address ++ "Mlt " ++ numStr p1 ++ numStr p2 ++ " → " ++ addrStr (address + 3) ++ "\n") :: output)
                        (writeOp op address comp)

                Input p1 ->
                    run (address + 2)
                        ((addrStr address ++ " " ++ numStr comp.inputStore ++ "→ INP → " ++ addrStr p1 ++ "\n") :: output)
                        (writeOp op address comp)

                Output p1 ->
                    run (address + 2)
                        ((addrStr address ++ "OUT → " ++ numStr p1 ++ "\n") :: output)
                        (writeOp op address comp)

                Halt ->
                    (addrStr address ++ "HLT\n") :: output |> List.reverse
    in
    run 0 []
```

---

### Examples

```elm {l r}
test1 : Int
test1 =
    [ 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 ]
        |> initComputer 0
        |> runProg 9 10
```

```elm {l m}
test2 : List String
test2 =
    [ 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 ]
        |> initComputer 0
        |> debug
```

```elm {l m}
test3 : List String
test3 =
    let
        comp =
            [ 1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 1, 10, 19, 1, 19, 5, 23, 2, 23, 9, 27, 1, 5, 27, 31, 1, 9, 31, 35, 1, 35, 10, 39, 2, 13, 39, 43, 1, 43, 9, 47, 1, 47, 9, 51, 1, 6, 51, 55, 1, 13, 55, 59, 1, 59, 13, 63, 1, 13, 63, 67, 1, 6, 67, 71, 1, 71, 13, 75, 2, 10, 75, 79, 1, 13, 79, 83, 1, 83, 10, 87, 2, 9, 87, 91, 1, 6, 91, 95, 1, 9, 95, 99, 2, 99, 10, 103, 1, 103, 5, 107, 2, 6, 107, 111, 1, 111, 6, 115, 1, 9, 115, 119, 1, 9, 119, 123, 2, 10, 123, 127, 1, 127, 5, 131, 2, 6, 131, 135, 1, 135, 5, 139, 1, 9, 139, 143, 2, 143, 13, 147, 1, 9, 147, 151, 1, 151, 2, 155, 1, 9, 155, 0, 99, 2, 0, 14, 0 ]
                |> initComputer 0
    in
    { comp
        | mem =
            comp.mem
                |> Dict.insert 1 12
                |> Dict.insert 2 2
    }
        |> debug
```

This should place 123 in input, store it at address 50 and then send the value at that address to output.

```elm {l m}
test4 : List String
test4 =
    [ 3, 50, 4, 50, 99 ]
        |> initComputer 123
        |> debug
```

```

```
