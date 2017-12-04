{- A new system policy has been put in place that requires all accounts to use a
   passphrase instead of simply a password. A passphrase consists of a series of
   words (lowercase letters) separated by spaces.

   To ensure security, a valid passphrase must contain no duplicate words.

   For example:

   aa bb cc dd ee is valid.
   aa bb cc dd aa is not valid - the word aa appears more than once.
   aa bb cc dd aaa is valid - aa and aaa count as different words.

   The system's full passphrase list is available as your puzzle input. How many
   passphrases are valid?

   --- Part Two ---

   For added security, yet another system policy has been put in place. Now, a valid
   passphrase must contain no two words that are anagrams of each other - that is,
   a passphrase is invalid if any word's letters can be rearranged to form any other
   word in the passphrase.

   For example:

   abcde fghij is a valid passphrase.
   abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
   a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
   iiii oiii ooii oooi oooo is valid.
   oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.

   Under this new system policy, how many passphrases are valid?
-}


module D04_2017 exposing (..)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat)
import Regex exposing (regex)
import Set exposing (Set)


main : Program Never Model Msg
main =
    aoc "data/d04_2017.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 =
    List.map parseLine
        >> numWithUnique


part2 : List String -> Int
part2 =
    List.map parseLine
        >> List.map (\line -> List.map sortChars line)
        >> numWithUnique


numWithUnique : List (List String) -> Int
numWithUnique =
    List.filter (\ws -> List.length ws == List.length (unique ws))
        >> List.length


unique : List comparable -> List comparable
unique =
    Set.fromList >> Set.toList


sortChars : String -> String
sortChars =
    String.toList >> List.sort >> String.fromList


parseLine : String -> List String
parseLine text =
    Regex.split Regex.All (regex "\\s+") text
