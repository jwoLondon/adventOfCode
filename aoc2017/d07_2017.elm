{- Wandering further through the circuits of the computer, you come upon a tower
   of programs that have gotten themselves into a bit of trouble. A recursive algorithm
   has gotten out of hand, and now they're balanced precariously in a large tower.

   One program at the bottom supports the entire tower. It's holding a large disc,
   and on the disc are balanced several more sub-towers. At the bottom of these
   sub-towers, standing on the bottom disc, are other programs, each holding their
   own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many programs
   stand simply keeping the disc below them balanced but with no disc of their own.

   You offer to help, but first you need to understand the structure of these towers.
   You ask each program to yell out their name, their weight, and (if they're holding
   a disc) the names of the programs immediately above them balancing on that disc.
   You write this information down (your puzzle input). Unfortunately, in their
   panic, they don't do this in an orderly fashion; by the time you're done, you're
   not sure which program gave which information.

   For example, if your list is the following:

   pbga (66)
   xhth (57)
   ebii (61)
   havc (66)
   ktlj (57)
   fwft (72) -> ktlj, cntj, xhth
   qoyq (66)
   padx (45) -> pbga, havc, qoyq
   tknk (41) -> ugml, padx, fwft
   jptl (61)
   ugml (68) -> gyxo, ebii, jptl
   gyxo (61)
   cntj (57)

   ...then you would be able to recreate the structure of the towers that looks like this:

                   gyxo
                 /
            ugml - ebii
          /      \
         |         jptl
         |
         |         pbga
        /        /
   tknk --- padx - havc
        \        \
         |         qoyq
         |
         |         ktlj
          \      /
            fwft - cntj
                 \
                   xhth

   In this example, tknk is at the bottom of the tower (the bottom program), and
   is holding up ugml, padx, and fwft. Those programs are, in turn, holding up other
   programs; in this example, none of those programs are holding up any other programs,
   and are all the tops of their own towers. (The actual tower balancing in front
   of you is much larger.)

   Before you're ready to help them, you need to make sure your information is correct.
   What is the name of the bottom program?

   --- Part Two ---

   The programs explain the situation: they can't get down. Rather, they could get
   down, if they weren't expending all of their energy trying to keep the tower
   balanced. Apparently, one program has the wrong weight, and until it's fixed,
   they're stuck here.

   For any program holding a disc, each program standing on that disc forms a sub-tower.
   Each of those sub-towers are supposed to be the same weight, or the disc itself
   isn't balanced. The weight of a tower is the sum of the weights of the programs
   in that tower.

   In the example above, this means that for ugml's disc to be balanced, gyxo, ebii,
   and jptl must all have the same weight, and they do: 61.

   However, for tknk to be balanced, each of the programs standing on its disc and
   all programs above it must each match. This means that the following sums must
   all be the same:

   ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
   padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
   fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243

   As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the other
   two. Even though the nodes above ugml are balanced, ugml itself is too heavy:
   it needs to be 8 units lighter for its stack to weigh 243 and keep the towers
   balanced. If this change were made, its weight would be 60.

   Given that exactly one program is the wrong weight, what would its weight need
   to be to balance the entire tower?
-}


module D07_2017 exposing (Prog(..), buildTree, correctWeight, main, parseForChildren, parseForWeight, part1, part2, unbalancedProg)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, submatches, toInt)
import Dict exposing (Dict)


type Prog
    = Node String Int (List Prog)


main : Program () Model Msg
main =
    aoc "data/d07_2017.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> String
part1 input =
    let
        ( parents, children ) =
            List.map parseForChildren input |> List.unzip
    in
    parents
        |> List.filter (\p -> List.member p (List.concat children) |> not)
        |> List.head
        |> Maybe.withDefault "No root found"


part2 : List String -> Int
part2 input =
    let
        weightLookup =
            List.map parseForWeight input |> Dict.fromList

        childrenLookup =
            List.map parseForChildren input |> Dict.fromList

        getWeight name =
            Dict.get name weightLookup |> Maybe.withDefault 0

        getChildren name =
            Dict.get name childrenLookup |> Maybe.withDefault []
    in
    input
        |> part1
        |> buildTree getWeight getChildren
        |> unbalancedProg
        |> correctWeight


buildTree : (String -> Int) -> (String -> List String) -> String -> Prog
buildTree att children name =
    Node name (att name) (List.map (buildTree att children) (children name))


unbalancedProg : Prog -> Maybe ( List Int, List Int )
unbalancedProg prog =
    let
        (Node _ _ children) =
            prog

        totalWeight (Node _ weight c) =
            weight + (List.map totalWeight c |> List.sum)

        cWeights =
            List.map totalWeight children

        identical ws =
            case ws of
                hd :: tl ->
                    List.all (\w -> w == hd) tl

                _ ->
                    True
    in
    if identical cWeights then
        Nothing

    else if List.filterMap unbalancedProg children == [] then
        Just ( cWeights, List.map (\(Node _ w _) -> w) children )

    else
        List.filterMap unbalancedProg children
            |> List.head


correctWeight : Maybe ( List Int, List Int ) -> Int
correctWeight unbalanced =
    let
        outlierDiff x xs =
            if (List.filter (\n -> n == x) xs |> List.length) == 1 then
                ((List.sum xs - x) // (List.length xs - 1)) - x

            else
                0
    in
    case unbalanced of
        Just wsPair ->
            List.map2 (\a b -> ( a, b )) (Tuple.first wsPair) (Tuple.second wsPair)
                |> List.map (\( w0, w1 ) -> ( outlierDiff w0 (Tuple.first wsPair), w1 ))
                |> List.filter (\( a, _ ) -> a /= 0)
                |> List.map (\( a, b ) -> a + b)
                |> List.head
                |> Maybe.withDefault 0

        Nothing ->
            0


parseForWeight : String -> ( String, Int )
parseForWeight =
    let
        toProg tokens =
            case tokens of
                [ prog, weight ] ->
                    ( prog, toInt weight )

                _ ->
                    ( String.concat tokens, 0 ) |> Debug.log "Bad input"
    in
    submatches "(\\w+) \\((\\d+)\\)"
        >> List.filterMap identity
        >> toProg


parseForChildren : String -> ( String, List String )
parseForChildren =
    let
        toProg tokens =
            case tokens of
                parent :: children ->
                    ( parent, children )

                _ ->
                    ( String.concat tokens, [] ) |> Debug.log "Bad input"
    in
    submatches "(\\w+) \\((?:\\d+)\\)|(?: -> )?(\\w+)"
        >> List.filterMap identity
        >> toProg
