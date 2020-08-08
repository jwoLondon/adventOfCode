module Aoc exposing
    ( Grid
    , GridLocation
    , SPFGraph
    , addCostToGoal
    , addCostsToGoal
    , addDirectedEdge
    , addDirectedEdges
    , addNToFreqTable
    , addToFreqTable
    , addUndirectedEdge
    , addUndirectedEdges
    , circularNeighbours
    , combinations
    , contains
    , curry
    , decToBinary
    , decToHex
    , dropWhile
    , edges
    , factors
    , flip
    , gColCount
    , gFromList
    , gFromLists
    , gGet
    , gGetCol
    , gGetRow
    , gInit
    , gMap
    , gMapWithLocation
    , gRowCount
    , gSet
    , gSetCol
    , gSetRow
    , gToList
    , gToLists
    , gTranspose
    , gridLocations
    , groupsOf
    , hexToBinary
    , highestCommonFactor
    , indexOf
    , iterate
    , lowestCommonMultiple
    , makeCycle
    , mapHeadDeque
    , mapTriplet
    , match
    , mode
    , modeCount
    , neighbours
    , nodes
    , pairwiseCombinations
    , permutations
    , replace
    , replaceFn
    , rotateDeque
    , rotateList
    , scanl
    , select
    , selectLargest
    , selectSplit
    , sequenceCycle
    , setListAt
    , shortestPath
    , split
    , splitAt
    , submatches
    , takeWhile
    , toInt
    , transpose
    , triplet
    , tripletFirst
    , tripletFromList
    , tripletSecond
    , tripletThird
    , uncurry
    , unique
    , unzip3
    , updateInsert
    , zip
    , zip3
    )

import AStar.Generalised
import Array
import Bitwise
import BoundedDeque exposing (BoundedDeque)
import Deque exposing (Deque)
import Dict exposing (Dict)
import Graph exposing (Graph)
import Matrix
import Regex
import Set exposing (Set)


type alias Grid a =
    Matrix.Matrix a


type alias GridLocation =
    Matrix.Location


{-| A graph structure amenable to shortest path first (SPF) searches (Dijkstra
and A\*). Nodes should be identified with unique comparable ids and edge traversal
costs as `Floats`. May optionally contain a cost to goal value for each node in
order to facilitate A\* searches (if not provided, will use Dijkstra).
-}
type alias SPFGraph comparable =
    Graph comparable Float Float


{-| Add the cost of getting from the given node to the goal in an SPF graph. This
can be an underestimate of the actual cost (i.e. sum of the shortest path edge
traversal), but should not overestimate. This is the 'heuristic' used in the A\*
search. If not provided, a simpler, but potentially less efficient Dijkstra search
will be used when calling [shortestPath](#shortestPath).
-}
addCostToGoal : comparable -> Float -> SPFGraph comparable -> SPFGraph comparable
addCostToGoal =
    Graph.insertData


{-| Add the costs of getting from the given nodes to the goal in an SPF graph.
These can be underestimates of the actual cost (i.e. sum of the shortest path edge
traversal), but should not overestimate. These provide the 'heuristic' used in the
A\* search. If not provided, a simpler, but potentially less efficient Dijkstra
search will be used when calling [shortestPath](#shortestPath).
-}
addCostsToGoal : List ( comparable, Float ) -> SPFGraph comparable -> SPFGraph comparable
addCostsToGoal =
    List.foldl (\( n, d ) -> addCostToGoal n d) |> flip


{-| Add a directed edge between two nodes in an SPF graph along with its
associated traversal cost. If the nodes do not already exist, they will be added
to the graph.
-}
addDirectedEdge : comparable -> comparable -> Float -> SPFGraph comparable -> SPFGraph comparable
addDirectedEdge =
    Graph.insertEdgeData


{-| Add a list of directed edges, each of which is between between two nodes
with an associated traversal cost. If any of the nodes do not already exist, they
will be added to the graph.
-}
addDirectedEdges : List ( comparable, comparable, Float ) -> SPFGraph comparable -> SPFGraph comparable
addDirectedEdges =
    flip (List.foldl (\( n1, n2, w ) -> addDirectedEdge n1 n2 w))


{-| Accumulate frequencies in a frequency table.
-}
addNToFreqTable : comparable -> Int -> Dict comparable Int -> Dict comparable Int
addNToFreqTable item n freqTable =
    if Dict.member item freqTable then
        Dict.update item (Maybe.map ((+) n)) freqTable

    else
        Dict.insert item n freqTable


{-| Accumulate frequencies in a frequency table one by one.
-}
addToFreqTable : comparable -> Dict comparable Int -> Dict comparable Int
addToFreqTable item freqTable =
    addNToFreqTable item 1 freqTable


{-| Add an undirected edge between two nodes in an SPF graph along with its
associated traversal cost. If the nodes do not already exist, they will be added
to the graph.
-}
addUndirectedEdge : comparable -> comparable -> Float -> SPFGraph comparable -> SPFGraph comparable
addUndirectedEdge n1 n2 traversalCost =
    Graph.insertEdgeData n1 n2 traversalCost
        >> Graph.insertEdgeData n2 n1 traversalCost


{-| Add a list of undirected edges to an SPF graph, each of which is between
between two nodes with an associated traversal cost. If any of the nodes do not
already exist, they will be added to the graph.
-}
addUndirectedEdges : List ( comparable, comparable, Float ) -> SPFGraph comparable -> SPFGraph comparable
addUndirectedEdges =
    flip (List.foldl (\( n1, n2, w ) -> addUndirectedEdge n1 n2 w))


{-| Create a circular list of adjacent neighbour tuples from a list where the
first item is the previous (circular) list item.

    circularNeighbours [1,2,3,4] == [(4,1), (1,2), (2,3), (3,4))]

-}
circularNeighbours : List a -> List ( a, a )
circularNeighbours items =
    neighbours (List.take 1 (List.reverse items) ++ items)


{-| Generate all combinations of size k or smaller of an ordered list.
-}
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


{-| Indicate whether the text in the second parameter contains any matches given
by the given regular expression (first parameter)
-}
contains : String -> String -> Bool
contains regex =
    Regex.contains (Regex.fromString regex |> Maybe.withDefault Regex.never)


{-| Split paired arguments into two separate ones.
-}
curry : (( a, b ) -> c) -> a -> b -> c
curry f a b =
    f ( a, b )


{-| Provide a binary representation of the given decimal. Represented as a list
of 1s and 0s with a minimum length determined by the padding parameter.
-}
decToBinary : Int -> Int -> List Int
decToBinary padding dec =
    let
        d2b pad bin d =
            if d == 0 then
                List.repeat (max 0 (padding - List.length bin)) 0 ++ bin

            else
                let
                    bit =
                        if modBy 2 d == 0 then
                            0

                        else
                            1
                in
                d2b pad (bit :: bin) (d // 2)
    in
    d2b padding [] dec


{-| Provide a decimal to hex conversion with the given number of digits (padded
with 0s if necessary).

    decToHex 4 510 == "01fe"`
    decToHex 8 65535 == "0000ffff"`

-}
decToHex : Int -> Int -> String
decToHex numDigits n =
    let
        hexChr x =
            [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' ]
                |> List.drop x
                |> List.head
                |> Maybe.withDefault 'f'

        convertDigit d chrs =
            hexChr (Bitwise.and (Bitwise.shiftRightBy (d * 4) n) 0x0F) :: chrs
    in
    List.foldl convertDigit [] (List.range 0 (numDigits - 1))
        |> String.fromList


{-| From [List.Extra](http://package.elm-lang.org/packages/elm-community/list-extra/latest),
drop items from the front of a list while the given condition is true.
-}
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


{-| A list of all the edges in an SPF graph and associated traversal costs.
-}
edges : SPFGraph comparable -> List ( comparable, comparable, Float )
edges =
    Graph.edgesWithData >> List.map (\( a, b, c ) -> ( a, b, c |> Maybe.withDefault 0 ))


{-| List of all the factors of a given number.
-}
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
            round (sqrt (toFloat (abs num)))
    in
    fac (abs num) upper []


{-| Reverse the order of parameters in a two-parameter function.
-}
flip : (a -> b -> c) -> b -> a -> c
flip fn argB argA =
    fn argA argB


{-| Number of columns in a given 2d grid. If location outside of grid `Nothing` is
returned, otherwise `Just` a list of the grid column values.
-}
gColCount : Grid a -> Int
gColCount =
    gToLists >> List.map List.length >> List.maximum >> Maybe.withDefault 0


{-| Create a 2d grid with the given number of columns from the given 1d list.
-}
gFromList : Int -> List a -> Grid a
gFromList numCols =
    groupsOf numCols
        >> Matrix.fromList


{-| Create a 2d grid from the given list of lists.
-}
gFromLists : List (List a) -> Grid a
gFromLists =
    Matrix.fromList


{-| Report the value at the given position in a 2d grid. If location outside of grid
`Nothing` is returned, otherwise `Just` the grid cell value.
-}
gGet : GridLocation -> Grid a -> Maybe a
gGet loc =
    Matrix.get loc


{-| Report the values in a given column position in a 2d grid.
-}
gGetCol : Int -> Grid a -> Maybe (List a)
gGetCol x =
    -- TODO: Can make this more efficient by modifying transpose to return the relevant column?
    gTranspose >> gGetRow x


{-| Report the values in a given row position in a 2d grid.
-}
gGetRow : Int -> Grid a -> Maybe (List a)
gGetRow y =
    Array.get y >> Maybe.map Array.toList


{-| Create a 2d grid of the given dimensions initially filled with the given value.
-}
gInit : Int -> Int -> a -> Grid a
gInit rowCount colCount =
    always >> Matrix.matrix rowCount colCount


{-| Apply a mapping function to every element in a 2d grid.
-}
gMap : (a -> b) -> Grid a -> Grid b
gMap =
    Matrix.map


{-| Apply a mapping function to every element in the grid with the option of
using of the row,column values of the grid cell location.
-}
gMapWithLocation : (GridLocation -> a -> b) -> Grid a -> Grid b
gMapWithLocation =
    Matrix.mapWithLocation


{-| Generate a list of coordinate pairs between the given minimum and maximum
positions inclusive.

    gridLocations ( 0, 0 ) ( 2, 2 ) == [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]

-}
gridLocations : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
gridLocations ( minX, minY ) ( maxX, maxY ) =
    List.range minX maxX
        |> List.concatMap (\x -> List.map (\y -> ( x, y )) (List.range minY maxY))


{-| From [List.Extra](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#groupsOf),
split a list into groups of a given size. If there are not enough elements to
completely fill the last group, it will not be included.

For example:

    groupsOf 3 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] == [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

-}
groupsOf : Int -> List a -> List (List a)
groupsOf size xs =
    let
        thisGroup =
            List.take size xs
    in
    if size > 0 && List.length thisGroup == size then
        thisGroup :: groupsOf size (List.drop size xs)

    else
        []


{-| Number of rows in a given 2d grid. If location outside of grid `Nothing` is
returned, otherwise `Just` a list of the grid row values.
-}
gRowCount : Grid a -> Int
gRowCount =
    Matrix.rowCount


{-| Set the value at the given position in a 2d grid.
-}
gSet : GridLocation -> a -> Grid a -> Grid a
gSet loc value =
    Matrix.set loc value


{-| Set the values at a given column position in a 2d grid. If the size of the given
list does not match the grid size, nothing is changed.
-}
gSetCol : Int -> List a -> Grid a -> Grid a
gSetCol c col =
    gTranspose >> gSetRow c col >> gTranspose


{-| Set the values at a given row position in a 2d grid. If the size of the given
list does not match the grid size, nothing is changed.
-}
gSetRow : Int -> List a -> Grid a -> Grid a
gSetRow r row g =
    if r < 0 || r >= Matrix.rowCount g || List.length row /= Matrix.colCount g then
        g

    else
        Array.set r (Array.fromList row) g


{-| Convert a 2d grid into a 1d list.
-}
gToList : Grid a -> List a
gToList =
    Matrix.flatten


{-| Convert a 2d grid into a list of lists, preserving rows and columns.
-}
gToLists : Grid a -> List (List a)
gToLists =
    Matrix.toList


{-| Transpose the row and column values in a 2d grid.
-}
gTranspose : Grid a -> Grid a
gTranspose =
    Matrix.toList >> transpose >> Matrix.fromList


{-| Provide a binary representation of the given hexadecimal number. Represented
as a list of 1s and 0s.
-}
hexToBinary : String -> List Int
hexToBinary hexStr =
    let
        hexLookup =
            List.map2 (\a b -> ( a, b ))
                ("0123456789abcdef" |> String.toList)
                (List.map (decToBinary 4) (List.range 0 15))
                |> Dict.fromList

        toBits hexChr =
            Dict.get hexChr hexLookup |> Maybe.withDefault []
    in
    hexStr
        |> String.toList
        |> List.foldl (\c digits -> digits ++ toBits c) []


{-| Highest common factor (HCF) of two integers. Note that for convenience, this
also returns a value when both numbers are 0 (1), when one is zero (the non-zero
number) and when values are negative.
-}
highestCommonFactor : Int -> Int -> Int
highestCommonFactor a b =
    case ( a, b ) of
        ( 0, 0 ) ->
            1

        ( n, 0 ) ->
            abs n

        ( 0, n ) ->
            abs n

        _ ->
            Set.intersect (Set.fromList (factors a)) (Set.fromList (factors b))
                |> Set.toList
                |> List.maximum
                |> Maybe.withDefault -1


{-| Find the index of the first occurrence of a value in a list.
-}
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


{-| Oterate something _n_ times (convenience function that folds over a counter
list).
-}
iterate : Int -> b -> (b -> b) -> b
iterate n initial fn =
    List.foldl (always fn) initial (List.range 1 n)


{-| Lowest common multiple (LCM) of a pair of numbers. For example,

    lowestCommonMultiple 4 6 == 12

To find the LCM of a list of numbers, just sequentially calculate the LCM of pairs
from the list. For example, to find the LCM of 4 6 and 25, use

    List.foldl lowestCommonMultiple 1 [ 4, 6, 25 ]

-}
lowestCommonMultiple : Int -> Int -> Int
lowestCommonMultiple a b =
    (abs a // highestCommonFactor a b) * abs b


{-| For a list of lists, add the first element of each list to its end, making a
list of closed cycles.
-}
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


{-| Convenience function for changing the head of a deque. If the deque is empty,
an empty list is always returned. The first parameter is the mapping function.
For example to increment the first value in a deque of integers:

    mapHead ((+) 1) myDeque

-}
mapHeadDeque : (a -> a) -> Deque a -> Deque a
mapHeadDeque f deque =
    case Deque.popFront deque of
        ( Just h, deq ) ->
            Deque.pushFront (f h) deq

        ( Nothing, deq ) ->
            deq


{-| Transform all three values of a 3-tuple.
-}
mapTriplet : (a -> x) -> (b -> y) -> (c -> z) -> ( a, b, c ) -> ( x, y, z )
mapTriplet fn1 fn2 fn3 ( a, b, c ) =
    ( fn1 a, fn2 b, fn3 c )


{-| Provide a list of matches determined by the regex (first parameter) found in
the text of the second parameter This version is useful for simpler regular
expressions that do not group into sub-matches.
-}
match : String -> String -> List String
match regex =
    Regex.find (Regex.fromString regex |> Maybe.withDefault Regex.never)
        >> List.map .match


{-| Commonest value (mode) in a list. If item not present in list, returns `Nothing`.
-}
mode : List comparable -> Maybe comparable
mode =
    List.foldl addToFreqTable Dict.empty
        >> Dict.toList
        >> List.map (\( a, b ) -> ( b, a ))
        >> List.sort
        >> List.reverse
        >> List.map Tuple.second
        >> List.head


{-| Frequency of commonest value (mode) in a list. If item not present in list, returns `Nothing`.
-}
modeCount : List comparable -> Maybe Int
modeCount =
    List.foldl addToFreqTable Dict.empty
        >> Dict.values
        >> List.sort
        >> List.reverse
        >> List.head


{-| Create a list of adjacent neighbour tuples from a list. Will be one shorter
than the original list.

    neighbours [ 1, 2, 3, 4 ] == [ ( 1, 2 ), ( 2, 3 ), ( 3, 4 ) ]

-}
neighbours : List a -> List ( a, a )
neighbours items =
    List.map2 Tuple.pair items (List.tail items |> Maybe.withDefault [])


{-| A list of all the nodes in an SPF graph and associated costs to goal. If costs
to goal have not been provided in the graph, costs are assumed to be 0.
-}
nodes : SPFGraph comparable -> List ( comparable, Float )
nodes =
    Graph.nodes >> List.map (Tuple.mapSecond (Maybe.withDefault 0))


{-| Convenience function for choosing pairwise combinations and returning the
results as a list of tuples
-}
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


{-| From [List.Extra](http://package.elm-lang.org/packages/elm-community/list-extra/6.1.0/List-Extra),
list of of all permutations of a list. The result is in lexicographic order.

    permutations [ 1, 2, 3 ] == [ [ 1, 2, 3 ], [ 1, 3, 2 ], [ 2, 1, 3 ], [ 2, 3, 1 ], [ 3, 1, 2 ], [ 3, 2, 1 ] ]

-}
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


{-| Search using a given regex (first parameter) replacing matches with the second
parameter applying it to the text of the third parameter.
-}
replace : String -> String -> String -> String
replace searchText replaceText =
    Regex.replace (Regex.fromString searchText |> Maybe.withDefault Regex.never) (\_ -> replaceText)


{-| Replace with a given match function. It is syntactic sugar for
[Regex.replace](https://package.elm-lang.org/packages/elm/regex/latest/Regex#replace).
-}
replaceFn : String -> (Regex.Match -> String) -> String -> String
replaceFn searchText =
    Regex.replace (Regex.fromString searchText |> Maybe.withDefault Regex.never)


{-| Rotate the values in a circular deque either forward (positive values of n)
or backward (negative values of n).
-}
rotateDeque : Int -> Deque a -> Deque a
rotateDeque n deque =
    let
        rotateClockwise d =
            let
                ( maybeVal, newDeque ) =
                    Deque.popBack d
            in
            case maybeVal of
                Just val ->
                    Deque.pushFront val newDeque

                Nothing ->
                    d

        rotateAnticlockwise d =
            let
                ( maybeVal, newDeque ) =
                    Deque.popFront d
            in
            case maybeVal of
                Just val ->
                    Deque.pushBack val newDeque

                Nothing ->
                    d
    in
    if n == 0 then
        deque

    else if n > 0 then
        List.foldl (\_ d -> rotateAnticlockwise d) deque (List.range 1 n)

    else
        List.foldl (\_ d -> rotateClockwise d) deque (List.range 1 (abs n))


{-| Take the last item in a list and put it at the front.

    rotateList [ 1, 2, 3, 4 ] == [ 4, 1, 2, 3 ]

-}
rotateList : List a -> List a
rotateList xs =
    List.take 1 (List.reverse xs) ++ List.take (List.length xs - 1) xs


{-| Fold over a list with an accumulating function. Result is a list comprising
each application of the function to the given input list. For example, to create
a list of the first ten triangular numbers:

    AOC.scanl (+) 1 (List.range 2 10)

-}
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


{-| From [List.Extra](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#select),
all combinations in the form of (e, rest of the list).

    select [ 1, 2, 3, 4 ] == [ ( 1, [ 2, 3, 4 ] ), ( 2, [ 1, 3, 4 ] ), ( 3, [ 1, 2, 4 ] ), ( 4, [ 1, 2, 3 ] ) ]

-}
select : List a -> List ( a, List a )
select xs =
    case xs of
        [] ->
            []

        x :: xTail ->
            ( x, xTail ) :: List.map (\( y, ys ) -> ( y, x :: ys )) (select xTail)


{-| All combinations in the form of (e, rest of the list) where e is larger than
all items in the rest of list.

    selectLargest [ 1, 2, 3, 4 ] == [ ( 1, [] ), ( 2, [ 1 ] ), ( 3, [ 1, 2 ] ), ( 4, [ 1, 2, 3 ] ) ]

-}
selectLargest : List comparable -> List ( comparable, List comparable )
selectLargest xs =
    case xs of
        [] ->
            []

        x :: xTail ->
            ( x, List.filter (\y -> y < x) xTail )
                :: List.map (\( y, ys ) -> ( y, x :: ys )) (selectLargest xTail)


{-| From [List.Extra](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#selectSplit),
all combinations in the form of (elements before, element, elements after).

    selectSplit [ 1, 2, 3, 4 ] == [ ( [], 1, [ 2, 3, 4 ] ), ( [ 1 ], 2, [ 3, 4 ] ), ( [ 1, 2 ], 3, [ 4 ] ), ( [ 1, 2, 3 ], 4, [] ) ]

-}
selectSplit : List a -> List ( List a, a, List a )
selectSplit list =
    case list of
        [] ->
            []

        x :: xs ->
            ( [], x, xs ) :: List.map (\( lys, y, rys ) -> ( x :: lys, y, rys )) (selectSplit xs)


{-| Detect possibly infinitely repeating sequences (cycles) of items in a sequence
generator. The repeated sequence may not start from the beginning of the list. This
uses [Brent's algorithm](https://en.wikipedia.org/wiki/Cycle_detection#Brent's_algorithm)
with the option of specifying a maximum length for cases where there is the possibility
of no repeats in an infinite sequence.

Given a maximum sequence length, a start value and a function that generates the
next value in a sequence, return the length of the cycle and the zero-indexed
position in the sequence of the first element of the cycle. If there is no cycle
detected less than the maximum sequence length, will return `Nothing`.

For example, suppose we have the function `\x -> x^2 + 1 |> modBy 255` (generating
the sequence `[3, 10, 101, 2, 5, 26, 167, 95, 101, 2, 5, 26, 167, 95, 101...]`).

Calling

    sequenceCycle 10 3 (\x -> x ^ 2 + 1 |> modBy 255)

will return `Just (6,2)` indicating the cycle is 6 items long starting at position
2 (zero-based) in the sequence.

Calling

    sequenceCycle 100 3 ((+) 1)

will return `Nothing` as there is no repeating sequence.

-}
sequenceCycle : Int -> a -> (a -> a) -> Maybe ( Int, Int )
sequenceCycle maxCycleLength x0 f =
    let
        findLambda tortoise hare lam p =
            if maxCycleLength > 0 && lam > maxCycleLength then
                Nothing

            else if tortoise == hare then
                Just lam

            else if p == lam then
                findLambda hare (f hare) 1 (p * 2)

            else
                findLambda tortoise (f hare) (lam + 1) p

        findMu tortoise hare mu =
            if tortoise == hare then
                mu

            else
                findMu (f tortoise) (f hare) (mu + 1)
    in
    case findLambda x0 (f x0) 1 1 of
        Just lambda ->
            Just ( lambda, findMu x0 (iterate lambda x0 f) 0 )

        Nothing ->
            Nothing


{-| Set the value of a list item at the given position. If the position is larger
than the list length, the value is appended to the end of the list. For algorithms
that do a lot of setting values at arbitrary positions, consider the more efficient
`Array` or `Deque` structures.
-}
setListAt : Int -> a -> List a -> List a
setListAt pos x xs =
    let
        ( l, r ) =
            splitAt pos xs
    in
    l ++ x :: (List.tail r |> Maybe.withDefault [])


{-| Calculate the shortest path between the two given nodes in an SPF graph. Result
is a list of node ids, inclusive of srart and end nodes or an empty list if no
path found.
-}
shortestPath : comparable -> comparable -> SPFGraph comparable -> List comparable
shortestPath start end graph =
    let
        cost g n1 n2 =
            case ( Graph.getEdgeData n1 n2 g, Graph.getData n2 g ) of
                ( Just c, Nothing ) ->
                    -- With no cost to goal, this is just a Dijkstra search
                    c

                ( Just c, Just cToGoal ) ->
                    -- With a cost to goal value we can do an A* search.
                    c + cToGoal

                ( Nothing, _ ) ->
                    -- No connection, so make cost very high
                    2 ^ 51 - 1
    in
    case AStar.Generalised.findPath (cost graph) (flip Graph.outgoing graph) start end of
        Just ns ->
            start :: ns

        Nothing ->
            []


{-| Split a string (second parameter) by patterns identified by a regex (first parameter).
-}
split : String -> String -> List String
split regex =
    Regex.split (Regex.fromString regex |> Maybe.withDefault Regex.never)


{-| Consistent with [List.Extra](http://package.elm-lang.org/packages/elm-community/list-extra/latest),
convenience function for splitting a list into two at the given partition point.
-}
splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


{-| Given a regex containing groups (first parameter), provide a list of sub (grouped)
matches found in the text of the second parameter. Allows regex groups to be
identified and where matched, will be `Just` a match or `Nothing` if the group
does not match.
-}
submatches : String -> String -> List (Maybe String)
submatches regex =
    Regex.find
        (Regex.fromString regex |> Maybe.withDefault Regex.never)
        >> List.concatMap .submatches


{-| Unsafe, but compact string to integer conversion for more readable code.
-}
toInt : String -> Int
toInt =
    String.toInt >> Maybe.withDefault 0


{-| From [List.Extra](http://package.elm-lang.org/packages/elm-community/list-extra/latest),
take items from the front of a list while the given condition is true. Like a
filter but acts sequentially and stops at the point of first `False` predicate.
-}
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


{-| Create a list of lists swapping `rows` and `columns` from an input list of
lists.
-}
transpose : List (List a) -> List (List a)
transpose xss =
    let
        numCols =
            List.head >> Maybe.withDefault [] >> List.length
    in
    List.foldr (List.map2 (::)) (List.repeat (numCols xss) []) xss


{-| Create a 3-tuple from three parameters.
-}
triplet : a -> b -> c -> ( a, b, c )
triplet a b c =
    ( a, b, c )


{-| The first value of a 3-tuple.
-}
tripletFirst : ( a, b, c ) -> a
tripletFirst ( a, _, _ ) =
    a


{-| Create a 3-tuple from the first three values in a list.
-}
tripletFromList : List a -> Maybe ( a, a, a )
tripletFromList xs =
    case xs of
        x1 :: x2 :: x3 :: tl ->
            Just ( x1, x2, x3 )

        _ ->
            Nothing


{-| The second value of a 3-tuple.
-}
tripletSecond : ( a, b, c ) -> b
tripletSecond ( _, b, _ ) =
    b


{-| The third value of a 3-tuple.
-}
tripletThird : ( a, b, c ) -> c
tripletThird ( _, _, c ) =
    c


{-| Combine two arguments into a single pair.
-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


{-| From [List.Extra](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#unique),
remove duplicate values, keeping the first instance of each element which appears
more than once. Unlike converting to and from a
[Set](https://package.elm-lang.org/packages/elm/core/latest/Set), this will preserve
the order of unique list items.

    unique [ 6, 2, 2, 1, 2, 4, 6 ] == [ 6, 2, 1, 4 ]

-}
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


{-| Unzip a list of 3-tuples into a 3-tuple of lists.

    unzip3 [ ( 1, 2, 3 ), ( 4, 5, 6 ) ] == ( [ 1, 4 ], [ 2, 5 ], [ 3, 6 ] )

-}
unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 triplets =
    let
        addTriplet ( x, y, z ) ( xs, ys, zs ) =
            ( x :: xs, y :: ys, z :: zs )
    in
    List.foldr addTriplet ( [], [], [] ) triplets


{-| Add an item to a dictionary if it does not exist, but replace an existing one
if it does.
-}
updateInsert : comparable -> a -> Dict comparable a -> Dict comparable a
updateInsert key val dict =
    if Dict.member key dict then
        Dict.update key (\v -> Just val) dict

    else
        Dict.insert key val dict


{-| Zip two lists together as a list of tuples.
-}
zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair


{-| Zip three lists together into a list of 3-tuples.
-}
zip3 : List a -> List b -> List c -> List ( a, b, c )
zip3 =
    List.map3 (\x y z -> ( x, y, z ))
