{- Santa is trying to deliver presents in a large apartment building, but he can't
   find the right floor - the directions he got are a little confusing. He starts
   on the ground floor (floor 0) and then follows the instructions one character
   at a time.

   An opening parenthesis, (, means he should go up one floor, and a closing parenthesis,
   ), means he should go down one floor.

   The apartment building is very tall, and the basement is very deep; he will never
   find the top or bottom floors.

   For example:

   (()) and ()() both result in floor 0.
   ((( and (()(()( both result in floor 3.
   ))((((( also results in floor 3.
   ()) and ))( both result in floor -1 (the first basement level).
   ))) and )())()) both result in floor -3.

   Part 1: To what floor do the instructions take Santa?

   Now, given the same instructions, find the position of the first character that
   causes him to enter the basement (floor -1). The first character in the instructions
   has position 1, the second character has position 2, and so on.

   For example:

   ) causes him to enter the basement at character position 1.
   ()()) causes him to enter the basement at character position 5.

   Part 2: What is the position of the character that causes Santa to first enter
   the basement?
-}


module D01_2015 exposing (indexOf, last, main, moveFloor, part1, part2)

import AdventOfCode exposing (Model, Msg, aoc, multiLineInput, outFormat, scanl)


main : Program () Model Msg
main =
    aoc "data/d01_2015.txt"
        (part1 >> outFormat |> multiLineInput)
        (part2 >> outFormat |> multiLineInput)


part1 : String -> Int
part1 =
    String.toList >> scanl moveFloor 0 >> last


part2 : String -> Int
part2 =
    String.toList >> scanl moveFloor 0 >> indexOf -1 0


moveFloor : Char -> Int -> Int
moveFloor instruction floor =
    if instruction == '(' then
        floor + 1

    else
        floor - 1


last : List Int -> Int
last =
    List.reverse >> List.head >> Maybe.withDefault 0


indexOf : a -> Int -> List a -> Int
indexOf item index list =
    case list of
        [] ->
            -1

        hd :: tl ->
            if hd == item then
                index

            else
                indexOf item (index + 1) tl
