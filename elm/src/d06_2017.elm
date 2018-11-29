{- A debugger program here is having an issue: it is trying to repair a memory
   reallocation routine, but it keeps getting stuck in an infinite loop.

   In this area, there are sixteen memory banks; each memory bank can hold any
   number of blocks. The goal of the reallocation routine is to balance the blocks
   between the memory banks.

   The reallocation routine operates in cycles. In each cycle, it finds the memory
   bank with the most blocks (ties won by the lowest-numbered memory bank) and
   redistributes those blocks among the banks. To do this, it removes all of the
   blocks from the selected bank, then moves to the next (by index) memory bank
   and inserts one of the blocks. It continues doing this until it runs out of
   blocks; if it reaches the last memory bank, it wraps around to the first one.

   The debugger would like to know how many redistributions can be done before a
   blocks-in-banks configuration is produced that has been seen before.

   For example, imagine a scenario with only four memory banks:

   * The banks start with 0, 2, 7, and 0 blocks. The third bank has the most blocks,
     so it is chosen for redistribution.
   * Starting with the next bank (the fourth bank) and then continuing to the first
     bank, the second bank, and so on, the 7 blocks are spread out over the memory
     banks. The fourth, first, and second banks get two blocks each, and the third
     bank gets one back. The final result looks like this: 2 4 1 2.
   * Next, the second bank is chosen because it contains the most blocks (four).
     Because there are four memory banks, each gets one block. The result is: 3 1 2 3.
   * Now, there is a tie between the first and fourth memory banks, both of which
     have three blocks. The first bank wins the tie, and its three blocks are
     distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
   * The fourth bank is chosen, and its four blocks are distributed such that each
     of the four banks receives one: 1 3 4 1.
   * The third bank is chosen, and the same thing happens: 2 4 1 2.

   At this point, we've reached a state we've seen before: 2 4 1 2 was already seen.
   The infinite loop is detected after the fifth block redistribution cycle, and
   so the answer in this example is 5.

   Given the initial block counts in your puzzle input, how many redistribution
   cycles must be completed before a configuration is produced that has been seen
   before?

   --- Part Two ---

   Out of curiosity, the debugger would also like to know the size of the loop:
   starting from a state that has already been seen, how many block redistribution
   cycles must be performed before that same state is seen again?

   In the example above, 2 4 1 2 is seen again after four cycles, and so the answer
   in that example would be 4.

   How many cycles are in the infinite loop that arises from the configuration in
   your puzzle input?
-}


module D06_2017 exposing (Banks, States, distribute, findRepeat, hash, indexedMax, main, part1, part2, splitLine)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat, toInt)
import Array exposing (Array)
import Set exposing (Set)


main : Program () Model Msg
main =
    aoc "../data/d06_2017.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)


type alias Banks =
    Array Int


type alias States =
    Set String


part1 : String -> Int
part1 input =
    findRepeat (splitLine input |> Array.fromList) Set.empty 0
        |> Tuple.second


part2 : String -> Int
part2 input =
    let
        firstRepeat =
            findRepeat (splitLine input |> Array.fromList) Set.empty 0
    in
    findRepeat (Tuple.first firstRepeat) Set.empty 0
        |> Tuple.second


findRepeat : Banks -> States -> Int -> ( Banks, Int )
findRepeat banks states counter =
    let
        state =
            hash banks
    in
    if Set.member state states then
        ( banks, counter )

    else
        findRepeat (distribute banks) (Set.insert state states) (counter + 1)


distribute : Banks -> Banks
distribute banks =
    let
        len =
            Array.length banks

        iMax =
            indexedMax banks

        maxVal =
            Tuple.second iMax

        divi =
            ceiling (toFloat maxVal / toFloat len)

        remains =
            if modBy divi maxVal > 0 then
                [ modBy divi maxVal ]
                    ++ List.repeat (len - maxVal // divi - 1) 0

            else
                List.repeat (len - maxVal // divi) 0

        rotate n xs =
            List.drop (len - modBy len n) xs ++ List.take (len - modBy len n) xs

        addition =
            List.repeat (maxVal // divi) divi
                ++ remains
                |> rotate (Tuple.first iMax + 1)
    in
    List.map2 (+) (Array.set (Tuple.first iMax) 0 banks |> Array.toList) addition
        |> Array.fromList


indexedMax : Banks -> ( Int, Int )
indexedMax banks =
    let
        findMax ( i, n ) ( iMax, nMax ) =
            if n > nMax then
                ( i, n )

            else
                ( iMax, nMax )
    in
    banks
        |> Array.indexedMap (\a b -> ( a, b ))
        |> Array.foldl findMax ( -1, 0 )


hash : Banks -> String
hash banks =
    banks
        |> Array.toList
        |> List.map (\n -> String.fromInt n ++ " ")
        |> String.concat


splitLine : String -> List Int
splitLine =
    String.split "\t" >> List.map toInt
