---
id: "litvis"

elm:
  dependencies:
    elm/regex: latest
    folkertdev/elm-deque: latest
    gicentre/elm-vegalite: latest
    HAN-ASD-DT/priority-queue: latest
    avh4/elm-fifo: latest

  source-directories:
    - src

narrative-schemas:
  - schemas/aoc
---

@import "css/aoc.less"

# Advent of Code utilities

This document contains utilities used across multiple advent of code documents. By linking with this one, common functions and imports can be added without cluttering the narrative of each page.

```elm {l=hidden}
import Array exposing (Array)
import Bitwise exposing (..)
import BoundedDeque exposing (BoundedDeque)
import Deque exposing (Deque)
import Dict exposing (Dict)
import Fifo exposing (Fifo)
import Json.Decode as JD
import MD5Fast as MD5
import Matrix exposing (..)
import PriorityQueue exposing (PriorityQueue)
import Regex
import Set exposing (Set)
import VegaLite as VL
```

## Parsing

Compact string to integer for more readable code.

```elm {l}
toInt : String -> Int
toInt =
    String.toInt >> Maybe.withDefault 0
```

Given a regex containing groups (first parameter), `submatches` will provide a list of sub (grouped) matches found in the text of the second parameter. Allows regex groups to be identified and where matched, will be `Just` a match or `Nothing` if the group does not match.

```elm {l}
submatches : String -> String -> List (Maybe String)
submatches regex =
    Regex.find
        (Regex.fromString regex |> Maybe.withDefault Regex.never)
        >> List.concatMap .submatches
```

Given a regex, `match` will provide a list of matches found in the text of the second parameter. This version is useful for simpler regular expressions that do not group into sub-matches.

```elm {l}
match : String -> String -> List String
match regex =
    Regex.find (Regex.fromString regex |> Maybe.withDefault Regex.never)
        >> List.map .match
```

Given a regular expression (first parameter), `contains` will indicate whether the text in the second parameter contains any matches.

```elm {l}
contains : String -> String -> Bool
contains regex =
    Regex.contains (Regex.fromString regex |> Maybe.withDefault Regex.never)
```

Thus will split a string (second parameter) by patterns identified by a regex (first parameter).

```elm {l}
split : String -> String -> List String
split regex =
    Regex.split (Regex.fromString regex |> Maybe.withDefault Regex.never)
```

`replace` will search using a given regex (first parameter) replacing matches with the second paramter applying it to the text of the third parameter.

```elm {l}
replace : String -> String -> String -> String
replace searchText replaceText =
    Regex.replace (Regex.fromString searchText |> Maybe.withDefault Regex.never) (\_ -> replaceText)
```

This version of replace allows a match function to be provided that generates the replace text.
It is syntatic sugar for [Regex.replace](https://package.elm-lang.org/packages/elm/regex/latest/Regex#replace).

```elm {l}
replaceFn : String -> (Regex.Match -> String) -> String -> String
replaceFn searchText =
    Regex.replace (Regex.fromString searchText |> Maybe.withDefault Regex.never)
```

## List Processing

Scanl was dropped from Elm 0.19, so it is added here for convenience:

```elm {l}
scanl : (a -> b -> b) -> b -> List a -> List b
scanl fn b =
    let
        scan a bs =
            case bs of
                hd :: tl ->
                    fn a hd :: bs

                _ ->
                    []
    in
    List.foldl scan [ b ] >> List.reverse
```

Zip two lists together as a list of tuples.

```elm {l}
zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair
```

From [List.Extra](http://package.elm-lang.org/packages/elm-community/list-extra/latest), drop items from the front of a list while the given condition is true.

```elm {l}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs

            else
                list
```

From [List.Extra](http://package.elm-lang.org/packages/elm-community/list-extra/latest), take items from the front of a list while the given condition is true. Like a filter but acts sequentially and stops at the point of first `False` predicate.

```elm {l}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate =
    let
        takeWhileHelper accum list =
            case list of
                [] ->
                    List.reverse accum

                x :: xs ->
                    if predicate x then
                        takeWhileHelper (x :: accum) xs

                    else
                        List.reverse accum
    in
    takeWhileHelper []
```

Find the index of the first occurance of a value in a list:

```elm {l}
indexOf : a -> List a -> Int
indexOf item list =
    let
        first index xs =
            case xs of
                [] ->
                    -1

                hd :: tl ->
                    if hd == item then
                        index

                    else
                        first (index + 1) tl
    in
    first 0 list
```

Consistent with [List.Extra](http://package.elm-lang.org/packages/elm-community/list-extra/latest),
convenience function for splitting a list into two at the given partition point.

```elm {l}
splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )
```

Create a list of lists swapping `rows` and `columns` from an input list of lists.

```elm {l}
transpose : List (List a) -> List (List a)
transpose xss =
    let
        numCols =
            List.head >> Maybe.withDefault [] >> List.length
    in
    List.foldr (List.map2 (::)) (List.repeat (numCols xss) []) xss
```

For a list of lists, add the first element of each list to its end, making a list of closed cycles.

```elm {l}
makeCycle : List (List a) -> List (List a)
makeCycle lists =
    let
        addHeadToTail list =
            case List.head list of
                Nothing ->
                    []

                Just hd ->
                    List.append list [ hd ]
    in
    List.map addHeadToTail lists
```

From [List.Extra](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#unique),
remove duplicate values, keeping the first instance of each element which appears more than once.
Unlike converting to and from a [Set](https://package.elm-lang.org/packages/elm/core/latest/Set), this will preserve the order of unique list items.

`unique [ 6, 2, 2, 1, 2, 4, 6 ] == [ 6, 2, 1, 4 ]`

```elm {l}
unique : List comparable -> List comparable
unique list =
    let
        uniqueHelp f existing remaining accumulator =
            case remaining of
                [] ->
                    List.reverse accumulator

                first :: rest ->
                    let
                        computedFirst =
                            f first
                    in
                    if Set.member computedFirst existing then
                        uniqueHelp f existing rest accumulator

                    else
                        uniqueHelp f (Set.insert computedFirst existing) rest (first :: accumulator)
    in
    uniqueHelp identity Set.empty list []
```

## Combinatorics

Generate all combinations of size _k_ or smaller of an ordered list.

```elm {l}
combinations : Int -> List a -> List (List a)
combinations k items =
    if k <= 0 then
        [ [] ]

    else
        case items of
            [] ->
                []

            hd :: tl ->
                let
                    appendedToAll item list =
                        List.map ((::) item) list
                in
                appendedToAll hd (combinations (k - 1) tl) ++ combinations k tl
```

A convenience function for chosing pairwise combinations and returning the results as a list of tuples:

```elm {l}
pairwiseCombinations : List a -> List ( a, a )
pairwiseCombinations =
    let
        toTuple list =
            case list of
                [ s1, s2 ] ->
                    Just ( s1, s2 )

                _ ->
                    Nothing
    in
    combinations 2 >> List.filterMap toTuple
```

Generate a list of coordinate pairs between the given minumum and maximum positions inclusive.

`gridLocations (0,0) (2,2) == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]`

```elm {l}
gridLocations : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
gridLocations ( minX, minY ) ( maxX, maxY ) =
    List.range minX maxX
        |> List.concatMap (\x -> List.map (\y -> ( x, y )) (List.range minY maxY))
```

From [List.Extra](http://package.elm-lang.org/packages/elm-community/list-extra/6.1.0/List-Extra), return the list of of all permutations of a list. The result is in lexicographic order.

`permutations [1,2,3] == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]`

```elm {l}
permutations : List a -> List (List a)
permutations xs_ =
    case xs_ of
        [] ->
            [ [] ]

        xs ->
            let
                f ( y, ys ) =
                    List.map ((::) y) (permutations ys)
            in
            List.concatMap f (select xs)
```

From [List.Extra](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#select), return all combinations in the form of (element, rest of the list).

`select [1,2,3,4] == [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]`

```elm {l}
select : List a -> List ( a, List a )
select xs =
    case xs of
        [] ->
            []

        x :: xTail ->
            ( x, xTail ) :: List.map (\( y, ys ) -> ( y, x :: ys )) (select xTail)
```

Return all combinations in the form of _(element, rest of the list)_ where element is larger than all items in the rest of list.

`selectLargest [1,2,3,4] == [(1,[]),(2,[1]),(3,[1,2]),(4,[1,2,3])]`

```elm {l}
selectLargest : List comparable -> List ( comparable, List comparable )
selectLargest xs =
    case xs of
        [] ->
            []

        x :: xTail ->
            ( x, List.filter (\y -> y < x) xTail )
                :: List.map (\( y, ys ) -> ( y, x :: ys )) (selectLargest xTail)
```

From [List.Extra](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#selectSplit), return all combinations in the form of (elements before, element, elements after).

`selectSplit [1,2,3,4] == [([],1,[2,3,4]), ([1],2,[3,4]), ([1,2],3,[4]), ([1,2,3],4,[])]`

```elm {l}
selectSplit : List a -> List ( List a, a, List a )
selectSplit list =
    case list of
        [] ->
            []

        x :: xs ->
            ( [], x, xs ) :: List.map (\( lys, y, rys ) -> ( x :: lys, y, rys )) (selectSplit xs)
```

## Frequency Distributions

Building a frequency distribution is a common task:

```elm {l}
addToFreqTable : comparable -> Dict comparable Int -> Dict comparable Int
addToFreqTable item freqTable =
    if Dict.member item freqTable then
        Dict.update item (Maybe.map ((+) 1)) freqTable

    else
        Dict.insert item 1 freqTable
```

Find the commonest value (mode) and its frequency in a list:

```elm {l}
mode : List comparable -> Maybe comparable
mode =
    List.foldl addToFreqTable Dict.empty
        >> Dict.toList
        >> List.map (\( a, b ) -> ( b, a ))
        >> List.sort
        >> List.reverse
        >> List.map Tuple.second
        >> List.head


modeCount : List comparable -> Maybe Int
modeCount =
    List.foldl addToFreqTable Dict.empty
        >> Dict.values
        >> List.sort
        >> List.reverse
        >> List.head
```

More generally, when we have a dictionary a common task is to add an item if it does not exist, but replace an existing one if it does:

```elm {l}
updateInsert : comparable -> a -> Dict comparable a -> Dict comparable a
updateInsert key val dict =
    if Dict.member key dict then
        Dict.update key (\v -> Just val) dict

    else
        Dict.insert key val dict
```

## Number Theory

Provides a list of all the factors of a given number.

```elm {l}
factors : Int -> List Int
factors num =
    let
        fac : Int -> Int -> List Int -> List Int
        fac n i facs =
            if i == 1 then
                1 :: n :: facs

            else if modBy i n == 0 then
                fac n (i - 1) (i :: (n // i) :: facs)

            else
                fac n (i - 1) facs

        upper =
            round (sqrt (toFloat num))
    in
    fac num upper []
```

## Number Conversion

Provide a binary representation of the given decimal. Represented as a list of 1s and 0s with a minumum length determined by the padding parameter.

```elm {l}
decToBinary : Int -> List Int -> Int -> List Int
decToBinary padding bin dec =
    if dec == 0 then
        List.repeat (max 0 (padding - List.length bin)) 0 ++ bin

    else
        let
            bit =
                if modBy 2 dec == 0 then
                    0

                else
                    1
        in
        decToBinary padding (bit :: bin) (dec // 2)
```

Provide a binary representation of the given hexidecimal number. Represented as a list of 1s and 0s.

```elm {l}
hexToBinary : String -> List Int
hexToBinary hexStr =
    let
        hexLookup =
            List.map2 (\a b -> ( a, b ))
                ("0123456789abcdef" |> String.toList)
                (List.map (decToBinary 4 []) (List.range 0 15))
                |> Dict.fromList

        toBits hexChr =
            Dict.get hexChr hexLookup |> Maybe.withDefault []
    in
    hexStr
        |> String.toList
        |> List.foldl (\c digits -> digits ++ toBits c) []
```

## Grid Manipulation

These are useful when values at particular grid locations need to be addressed. They wrap an Elm 0.19 version of `chendrix/elm-matrix` and add some convenience functions for manipulating entire rows and columns.

```elm {l}
type alias GridLocation =
    Matrix.Location


type alias Grid a =
    Matrix.Matrix a


gInit : Int -> Int -> a -> Grid a
gInit rowCount colCount =
    always >> Matrix.matrix rowCount colCount


gToList : Grid a -> List a
gToList =
    Matrix.flatten


gToLists : Grid a -> List (List a)
gToLists =
    Matrix.toList


gGet : GridLocation -> Grid a -> Maybe a
gGet loc =
    Matrix.get loc


gSet : GridLocation -> a -> Grid a -> Grid a
gSet loc value =
    Matrix.set loc value


gRowCount : Grid a -> Int
gRowCount =
    Matrix.rowCount


gColCount : Grid a -> Int
gColCount =
    Matrix.colCount


gGetRow : Int -> Grid a -> Maybe (List a)
gGetRow y =
    Array.get y >> Maybe.map Array.toList


gGetCol : Int -> Grid a -> Maybe (List a)
gGetCol x =
    -- TODO: Can make this more efficient by modifying transpose to return the relevant column?
    gTranspose >> gGetRow x


gSetRow : Int -> List a -> Grid a -> Grid a
gSetRow r row g =
    if r < 0 || r >= Matrix.rowCount g || List.length row /= Matrix.colCount g then
        g

    else
        Array.set r (Array.fromList row) g


gSetCol : Int -> List a -> Grid a -> Grid a
gSetCol c col =
    gTranspose >> gSetRow c col >> gTranspose


{-| Apply a mapping function to every element in the grid.
-}
gMap : (a -> b) -> Grid a -> Grid b
gMap =
    map


{-| Apply a mapping function to every element in the grid with the option of
using of the row,column values of the grid cell location.
-}
gMapWithLocation : (GridLocation -> a -> b) -> Grid a -> Grid b
gMapWithLocation =
    mapWithLocation


gTranspose : Grid a -> Grid a
gTranspose =
    let
        transposeLists ll =
            let
                heads =
                    List.filterMap List.head ll

                tails =
                    List.filterMap List.tail ll
            in
            if List.length heads == List.length ll then
                heads :: transposeLists tails

            else
                []
    in
    Matrix.toList >> transposeLists >> Matrix.fromList
```

### 3-Tuples

Elm has functions for creating and transforming tuples with two parameters, but not for three.
These functions provide the extra support for 3-tuples:

Create a 3-tuple

```elm {l}
triplet : a -> b -> c -> ( a, b, c )
triplet a b c =
    ( a, b, c )
```

The first value of a 3-tuple.

```elm {l}
tripletFirst : ( a, b, c ) -> a
tripletFirst ( a, _, _ ) =
    a
```

The second value of a 3-tuple.

```elm {l}
tripletSecond : ( a, b, c ) -> b
tripletSecond ( _, b, _ ) =
    b
```

The third value of a 3-tuple.

```elm {l}
tripletThird : ( a, b, c ) -> c
tripletThird ( _, _, c ) =
    c
```

Transform all three values of a 3-tuple:

`( "Lovelace", 1851, False ) |> mapTriplet String.toUpper ((+) 1) not`

```elm {l}
mapTriplet : (a -> x) -> (b -> y) -> (c -> z) -> ( a, b, c ) -> ( x, y, z )
mapTriplet fn1 fn2 fn3 ( a, b, c ) =
    ( fn1 a, fn2 b, fn3 c )
```

## Functional Utilities

Reverse the order of parameters in a two-parameter function (was removed in Elm 0.19).

```elm {l}
flip : (a -> b -> c) -> b -> a -> c
flip fn argB argA =
    fn argA argB
```

Sometimes, puzzles ask us to iterate something _n_ times.
This can be achieved by folding, supplying a list from 1..n, so this convenience function just makes that clearer:

```elm {l}
iterate : Int -> b -> (b -> b) -> b
iterate n initial fn =
    List.foldl (always fn) initial (List.range 1 n)
```
