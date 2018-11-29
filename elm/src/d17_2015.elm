{- The elves bought too much eggnog again - 150 liters this time. To fit it all
   into your refrigerator, you'll need to move it into smaller containers. You
   take an inventory of the capacities of the available containers.

   For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters.
   If you need to store 25 liters, there are four ways to do it:

   15 and 10
   20 and 5 (the first 5)
   20 and 5 (the second 5)
   15, 5, and 5

   Filling all containers entirely, how many different combinations of containers
   can exactly fit all 150 liters of eggnog?

   --- Part Two ---

   While playing with all the containers in the kitchen, another load of eggnog
   arrives! The shipping and receiving department is requesting as many containers
   as you can spare.

   Find the minimum number of containers that can exactly fit all 150 liters of
   eggnog. How many different ways can you fill that number of containers and still
   hold exactly 150 litres?

   In the example above, the minimum number of containers was two. There were three
   ways to use that many containers, and so the answer there would be 3.
-}


module D17_2015 exposing (allCombinations, knapsacks, main, maxNumItems, minNumItems, part1, part2)

import AdventOfCode exposing (Model, Msg, aoc, combinations, outFormat, scanl)


main : Program () Model Msg
main =
    aoc "../data/d17_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 input =
    let
        containers =
            List.filterMap String.toInt input
    in
    knapsacks (minNumItems 150 containers) (maxNumItems 150 containers) 150 containers
        |> List.length


part2 : List String -> Int
part2 input =
    let
        containers =
            List.filterMap String.toInt input

        min =
            minNumItems 150 containers
    in
    knapsacks min min 150 containers |> List.length


knapsacks : Int -> Int -> Int -> List Int -> List (List Int)
knapsacks minK maxK capacity items =
    let
        holds cap n =
            List.sum n == cap
    in
    List.filter (holds capacity) (allCombinations minK maxK items)


allCombinations : Int -> Int -> List a -> List (List a)
allCombinations minK k items =
    if k == minK then
        combinations k items

    else
        allCombinations minK (k - 1) items ++ combinations k items


minNumItems : Int -> List Int -> Int
minNumItems target list =
    list
        |> List.sort
        |> List.reverse
        |> scanl (+) 0
        |> List.filter (\x -> x <= target)
        |> List.length


maxNumItems : Int -> List Int -> Int
maxNumItems target list =
    (list
        |> List.sort
        |> scanl (+) 0
        |> List.filter (\x -> x <= target)
        |> List.length
    )
        - 1
