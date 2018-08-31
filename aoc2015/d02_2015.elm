{- The elves are running low on wrapping paper, and so they need to submit an order
   for more. They have a list of the dimensions (length l, width w, and height h)
   of each present, and only want to order exactly as much as they need.

   Fortunately, every present is a box (a perfect right rectangular prism), which
   makes calculating the required wrapping paper for each gift a little easier:
   find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves
   also need a little extra paper for each present: the area of the smallest side.

   For example:

   A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of
   wrapping paper plus 6 square feet of slack, for a total of 58 square feet.
   A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet
   of wrapping paper plus 1 square foot of slack, for a total of 43 square feet.
   All numbers in the elves' list are in feet. How many total square feet of
   wrapping paper should they order?

   --- Part Two ---

   The elves are also running low on ribbon. Ribbon is all the same width, so they
   only have to worry about the length they need to order, which they would again
   like to be exact.

   The ribbon required to wrap a present is the shortest distance around its sides,
   or the smallest perimeter of any one face. Each present also requires a bow made
   out of ribbon as well; the feet of ribbon required for the perfect bow is equal
   to the cubic feet of volume of the present. Don't ask how they tie the bow,
   though; they'll never tell.

   For example:

   A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap
   the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
   A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon to wrap
   the present plus 1*1*10 = 10 feet of ribbon for the bow, for a total of 14 feet.
   How many total feet of ribbon should they order?
-}


module D02_2015 exposing (main, paperArea, part1, part2, ribbonLength, smallestArea, smallestPerimeter)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, toInt)


main : Program () Model Msg
main =
    aoc "data/d02_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 =
    List.map paperArea >> List.sum


part2 : List String -> Int
part2 =
    List.map ribbonLength >> List.sum


paperArea : String -> Int
paperArea dimensionText =
    let
        dimensions =
            String.split "x" dimensionText |> List.map toInt
    in
    case dimensions of
        [ l, w, h ] ->
            2 * (l * w + w * h + h * l) + smallestArea [ l, w, h ]

        _ ->
            0


ribbonLength : String -> Int
ribbonLength dimensionText =
    let
        dimensions =
            String.split "x" dimensionText |> List.map toInt
    in
    case dimensions of
        [ l, w, h ] ->
            (l * w * h) + smallestPerimeter [ l, w, h ]

        _ ->
            0


smallestArea : List Int -> Int
smallestArea dimensions =
    let
        sorted =
            List.sort dimensions
    in
    case sorted of
        [ d1, d2, d3 ] ->
            d1 * d2

        _ ->
            0


smallestPerimeter : List Int -> Int
smallestPerimeter dimensions =
    let
        sorted =
            List.sort dimensions
    in
    case sorted of
        [ d1, d2, d3 ] ->
            2 * (d1 + d2)

        _ ->
            0
