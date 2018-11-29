{- Santa's previous password expired, and he needs help choosing a new one.

   To help him remember his new password after the old one expires, Santa has devised
   a method of coming up with a password based on the previous one. Corporate policy
   dictates that passwords must be exactly eight lowercase letters (for security
   reasons), so he finds his new password by incrementing his old password string
   repeatedly until it is valid.

   Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so on.
   Increase the rightmost letter one step; if it was z, it wraps around to a, and
   repeat with the next letter to the left until one doesn't wrap around.

   Unfortunately for Santa, a new Security-Elf recently started, and he has imposed
   some additional password requirements:

   Passwords must include one increasing straight of at least three letters, like
   abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
   Passwords may not contain the letters i, o, or l, as these letters can be mistaken
   for other characters and are therefore confusing.
   Passwords must contain at least two different, non-overlapping pairs of letters,
   like aa, bb, or zz.
   For example:

   * hijklmmn meets the first requirement (because it contains the straight hij)
     but fails the second requirement requirement (because it contains i and l).
   * abbceffg meets the third requirement (because it repeats bb and ff) but fails
     the first requirement.
   * abbcegjk fails the third requirement, because it only has one double letter
     (bb).
   * The next password after abcdefgh is abcdffaa.
   * The next password after ghijklmn is ghjaabcc, because you eventually skip all
     the passwords that start with ghi..., since i is not allowed.

   Given Santa's current password (your puzzle input), what should his next password be?

   --- Part Two ---

   Santa's password expired again. What's the next one?
-}


module D11_2015 exposing (hasNoIOL, hasStraight, hasTwoPairs, increment, main, nextPwd, nextValid, part1, part2)

import AdventOfCode exposing (Model, Msg, aoc, contains, multiLineInput, outFormat)
import Char


main : Program () Model Msg
main =
    aoc "../data/d11_2015.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)


part1 : String -> String
part1 input =
    nextValid (nextPwd input)


part2 : String -> String
part2 input =
    nextValid (nextPwd <| part1 input)


nextPwd : String -> String
nextPwd oldPwd =
    String.fromList (increment True (List.reverse <| String.toList oldPwd) [])


increment : Bool -> List Char -> List Char -> List Char
increment carry oldPwd newPwd =
    case oldPwd of
        [] ->
            newPwd

        hd :: tl ->
            if carry then
                if hd == 'z' then
                    increment True tl ('a' :: newPwd)

                else
                    increment False tl (Char.fromCode (Char.toCode hd + 1) :: newPwd)

            else
                increment False tl (hd :: newPwd)


nextValid : String -> String
nextValid pwd =
    let
        isValid =
            hasStraight pwd && hasTwoPairs pwd && hasNoIOL pwd
    in
    if isValid then
        pwd

    else
        nextValid (nextPwd pwd)


hasNoIOL : String -> Bool
hasNoIOL =
    not << contains "[iol]"


hasStraight : String -> Bool
hasStraight =
    contains "(abc|bcd|cde|def|efg|fgh|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz)"


hasTwoPairs : String -> Bool
hasTwoPairs =
    contains "(.)\\1.*((?!\\1).)\\2"
