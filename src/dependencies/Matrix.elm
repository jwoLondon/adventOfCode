module Matrix exposing
    ( Location, loc, row, col
    , Matrix
    , matrix, square, fromList
    , map, mapWithLocation, flatten
    , get, set, update
    , colCount, rowCount
    , toList
    )

{-| A library for creating and using 2-D matrices/grids. This is an update of
<https://github.com/chendrix/elm-matrix> for elm 0.19.


# Locations

@docs Location, loc, row, col


# Matrices

@docs Matrix


## Create

@docs matrix, square, fromList


## Transform

@docs map, mapWithLocation, flatten


## Get and Set

@docs get, set, update


## Properties

@docs colCount, rowCount


## Convert to other types

@docs toList

-}

import Array
import List


{-| An ordered collection of elements, all of a particular type, arranged into `m` rows and `n` columns.
-}
type alias Matrix a =
    Array.Array (Array.Array a)


{-| A representation of a row number and a column number, used to locate and access elements in a matrix.
-}
type alias Location =
    ( Int, Int )


{-| Turn two integers into a location
-}
loc : Int -> Int -> Location
loc =
    \x y -> ( x, y )


{-| Extract the row number from a location

    row (loc 3 5) == 3

-}
row : Location -> Int
row =
    Tuple.first


{-| Extract the col number from a location

    col (loc 3 5) == 5

-}
col : Location -> Int
col =
    Tuple.second


{-| Create a square matrix of a certain size

    square 2 (\_ -> 'H')
        == H H
            H
            H

-}
square : Int -> (Location -> a) -> Matrix a
square size =
    matrix size size


{-| Initialize a new matrix of size `m x n`.
Delegates to a function of type `Location -> a` to determine value to
place at each element in the matrix.

    matrix 3
        5
        (\location ->
            if isEven (row location) then
                "Hello"

            else
                "World"
        )

will give back the matrix

    Hello Hello Hello Hello Hello

    World World World World World

    Hello Hello Hello Hello Hello

-}
matrix : Int -> Int -> (Location -> a) -> Matrix a
matrix numRows numCols f =
    Array.initialize numRows
        (\r ->
            Array.initialize numCols
                (\c -> f (loc r c))
        )


{-| Apply the function to every element in the matrix

    map not (fromList [ [ True, False ], [ False, True ] ]) == fromList [ [ False, True ], [ True, False ] ]

-}
map : (a -> b) -> Matrix a -> Matrix b
map f m =
    Array.map (Array.map f) m


{-| Apply the function to every element in the list, where the first function argument
is the location of the element.

    let
        m =
            square 2 (\_ -> 1)

        f location element =
            if row location == col location then
                element * 2

            else
                element
    in
    mapWithLocation f m == fromList [ [ 2, 1 ], [ 1, 2 ] ]

-}
mapWithLocation : (Location -> a -> b) -> Matrix a -> Matrix b
mapWithLocation f m =
    Array.indexedMap
        (\rowNum r ->
            Array.indexedMap
                (\colNum element ->
                    f (loc rowNum colNum) element
                )
                r
        )
        m


{-| Convert a matrix to a list of lists

    toList (fromList [ [ 1, 0 ], [ 0, 1 ] ]) == [ [ 1, 0 ], [ 0, 1 ] ]

-}
toList : Matrix a -> List (List a)
toList m =
    Array.map Array.toList m
        |> Array.toList


{-| Convert a list of lists into a matrix

    fromList [ [ 1, 0 ], [ 0, 1 ] ]
        == square 2
            (\l ->
                if row l == col l then
                    1

                else
                    0
            )

-}
fromList : List (List a) -> Matrix a
fromList l =
    List.map Array.fromList l
        |> Array.fromList


{-| Convert a matrix to a single list

    let
        m =
            fromList [ [ 0, 1 ], [ 2, 3 ], [ 4, 5 ] ]
    in
    flatten m == [ 0, 1, 2, 3, 4, 5 ]

-}
flatten : Matrix a -> List a
flatten m =
    List.concat <| toList m


{-| Get the element at a particular location

    get (loc -1 1) (square 2 (\_ -> True)) == Nothing

    get (loc 1 1) (fromList [ [ 0, 1 ], [ 2, 3 ] ]) == Just 3

-}
get : Location -> Matrix a -> Maybe a
get location m =
    Array.get (row location) m |> Maybe.andThen (Array.get (col location))


{-| Set the element at a particular location

    set (loc -1 1) 42 (square 2 (\_ -> True)) == square 2 (\_ -> True)

    set (loc 1 1) 42 (fromList [ [ 0, 1 ], [ 2, 3 ] ]) == fromList [ [ 0, 1 ], [ 2, 42 ] ]

-}
set : Location -> a -> Matrix a -> Matrix a
set location value m =
    update location (always value) m


{-| Update the element at a particular location using the current value
-}
update : Location -> (a -> a) -> Matrix a -> Matrix a
update location f m =
    get location m
        |> Maybe.map
            (\current ->
                Array.get (row location) m
                    |> Maybe.map
                        (\oldRow ->
                            Array.set (col location) (f current) oldRow
                                |> (\newRow -> Array.set (row location) newRow m)
                        )
                    |> Maybe.withDefault m
            )
        |> Maybe.withDefault m


{-| Get the number of columns in a matrix
-}
colCount : Matrix a -> Int
colCount m =
    Array.get 0 m
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


{-| Get the number of rows in a matrix
-}
rowCount : Matrix a -> Int
rowCount m =
    Array.length m
