{- Today, you set out on the task of perfecting your milk-dunking cookie recipe.
   All you have to do is find the right balance of ingredients.

   Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a
   list of the remaining ingredients you could use to finish the recipe (your
   puzzle input) and their properties per teaspoon:

   capacity (how well it helps the cookie absorb milk)
   durability (how well it keeps the cookie intact when full of milk)
   flavor (how tasty it makes the cookie)
   texture (how it improves the feel of the cookie)
   calories (how many calories it adds to the cookie)

   You can only measure ingredients in whole-teaspoon amounts accurately, and you
   have to be accurate so you can reproduce your results in the future. The total
   score of a cookie can be found by adding up each of the properties (negative
   totals become 0) and then multiplying together everything except calories.

   For instance, suppose you have these two ingredients:

   Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
   Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3

   Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of cinnamon
   (because the amounts of each ingredient must add up to 100) would result in a
   cookie with the following properties:

   A capacity of 44*-1 + 56*2 = 68
   A durability of 44*-2 + 56*3 = 80
   A flavor of 44*6 + 56*-2 = 152
   A texture of 44*3 + 56*-1 = 76

   Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now) results
   in a total score of 62842880, which happens to be the best score possible given
   these ingredients. If any properties had produced a negative total, it would
   have instead become zero, causing the whole score to multiply to zero.

   Given the ingredients in your kitchen and their properties, what is the total
   score of the highest-scoring cookie you can make?

   --- Part Two ---

   Your cookie recipe becomes wildly popular! Someone asks if you can make another
   recipe that has exactly 500 calories per cookie (so they can use it as a meal
   replacement). Keep the rest of your award-winning process the same (100 teaspoons,
   same ingredients, same scoring system).

   For example, given the ingredients above, if you had instead selected 40 teaspoons
   of butterscotch and 60 teaspoons of cinnamon (which still adds to 100), the total
   calorie count would be 40*8 + 60*3 = 500. The total score would go down, though:
   only 57600000, the best you can do in such trying circumstances.

   Given the ingredients in your kitchen and their properties, what is the total
   score of the highest-scoring cookie you can make with a calorie total of 500?
-}


module D15_2015 exposing (Ingredient, Properties, main, next, parseLine, part1, part2, permute, totalScore)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, submatches, toInt, transpose)


type alias Ingredient =
    { cap : Int
    , dur : Int
    , fla : Int
    , tex : Int
    , cal : Int
    }


type alias Properties =
    List (List Int)


main : Program () Model Msg
main =
    aoc "../data/d15_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 input =
    let
        ingredients =
            List.foldl parseLine [] input

        allProps =
            List.map (\i -> [ i.cap, i.dur, i.fla, i.tex ]) ingredients |> transpose
    in
    permute allProps [] (ingredients |> List.length) 100 [] 0


part2 : List String -> Int
part2 input =
    let
        ingredients =
            List.foldl parseLine [] input

        allProps =
            List.map (\i -> [ i.cap, i.dur, i.fla, i.tex ]) ingredients |> transpose

        cals =
            List.map (\i -> i.cal) ingredients
    in
    permute allProps cals (ingredients |> List.length) 100 [] 0


permute : Properties -> List Int -> Int -> Int -> List Int -> Int -> Int
permute allProps calories qToFind unallocated quantities maxScore =
    if qToFind == 1 then
        max maxScore <|
            totalScore allProps calories (unallocated :: quantities)

    else
        next allProps calories (qToFind - 1) unallocated 0 quantities maxScore


next : Properties -> List Int -> Int -> Int -> Int -> List Int -> Int -> Int
next allProps calories qToFind unallocated allocated quantities maxScore =
    let
        newMax =
            max maxScore <|
                permute allProps calories qToFind unallocated (allocated :: quantities) maxScore
    in
    if unallocated > 0 then
        max maxScore <|
            next allProps calories qToFind (unallocated - 1) (allocated + 1) quantities newMax

    else
        newMax


totalScore : Properties -> List Int -> List Int -> Int
totalScore allProps calories quantities =
    let
        propScore q props =
            let
                properties =
                    List.map2 (*) props q
            in
            max 0 <|
                List.sum properties

        propScores =
            List.map (propScore quantities) allProps
    in
    if calories == [] || propScore quantities calories == 500 then
        List.product propScores

    else
        0


parseLine : String -> List Ingredient -> List Ingredient
parseLine text ingredients =
    let
        regex =
            "\\w+: capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)"
    in
    case submatches regex text of
        [ Just cap, Just dur, Just fla, Just tex, Just cal ] ->
            ingredients
                ++ [ Ingredient (toInt cap) (toInt dur) (toInt fla) (toInt tex) (toInt cal) ]

        _ ->
            ingredients
