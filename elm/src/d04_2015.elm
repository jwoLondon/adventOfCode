{- Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
   gifts for all the economically forward-thinking little girls and boys.

   To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
   least five zeroes. The input to the MD5 hash is some secret key (your puzzle
   input, given below) followed by a number in decimal. To mine AdventCoins, you
   must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
   that produces such a hash.

   For example:

   If your secret key is abcdef, the answer is 609043, because the MD5 hash of
   abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest
   such number to do so.
   If your secret key is pqrstuv, the lowest number it combines with to make an
   MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of
   pqrstuv1048970 looks like 000006136ef....

   --- Part Two ---
   Now find one that starts with six zeroes.
-}


module D04_2015 exposing (findNextZeroHash, hasLeadingZeros, main, part1, part2)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat)
import MD5Fast as MD5


main : Program () Model Msg
main =
    aoc "../data/d04_2015.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)


part1 : String -> Int
part1 =
    findNextZeroHash 5 1


part2 : String -> Int
part2 key =
    findNextZeroHash 6 (part1 key) key


findNextZeroHash : Int -> Int -> String -> Int
findNextZeroHash numZeros num key =
    let
        hash =
            key ++ String.fromInt num |> MD5.hex
    in
    if hasLeadingZeros numZeros hash then
        num

    else
        findNextZeroHash numZeros (num + 1) key


hasLeadingZeros : Int -> String -> Bool
hasLeadingZeros numZeros str =
    String.left numZeros str == String.left numZeros "00000000000000000000000"
