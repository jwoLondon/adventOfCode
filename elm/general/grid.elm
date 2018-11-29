module Grid exposing (Grid, Location, colCount, colIndices, flipCols, flipRows, fromList, get, getCol, getRow, gridIndices, init, rotate, rowCount, rowIndices, set, setCol, setRow, toCols, toList, toRows, transpose)

import Array exposing (Array)
import Matrix



{- This version wraps the Matrix module and adds row and column functions. -}


type alias Location =
    Matrix.Location


type alias Grid a =
    Matrix.Matrix a


init : Int -> Int -> a -> Grid a
init rCount cCount =
    always >> Matrix.matrix rCount cCount


fromList : a -> List (List a) -> Grid a
fromList default rows =
    let
        grid =
            init
                (rows |> List.length)
                (rows |> List.head |> Maybe.withDefault [] |> List.length)
                default
    in
    List.foldl (\xs ( row, g ) -> ( row + 1, setRow row xs g )) ( 0, grid ) rows
        |> Tuple.second


toRows : Grid a -> List (List a)
toRows =
    Matrix.toList


toCols : Grid a -> List (List a)
toCols =
    transpose >> Matrix.toList


toList : Grid a -> List a
toList =
    Matrix.flatten


get : Location -> Grid a -> Maybe a
get loc =
    Matrix.get loc


set : Location -> a -> Grid a -> Grid a
set loc value =
    Matrix.set loc value


rowCount : Grid a -> Int
rowCount =
    Matrix.rowCount


{-| Convenience function to generate a list of row indices for the grid
-}
rowIndices : Grid a -> List Int
rowIndices grid =
    List.range 0 (Matrix.rowCount grid - 1)


colCount : Grid a -> Int
colCount =
    Matrix.colCount


{-| Convenience function to generate a list of column indices for the grid
-}
colIndices : Grid a -> List Int
colIndices grid =
    List.range 0 (Matrix.colCount grid - 1)


{-| Convenience function to generate a (row,col) tuple for each cell in the grid
in row prime order.
-}
gridIndices : Grid a -> List ( Int, Int )
gridIndices grid =
    List.concatMap (\r -> List.map (\c -> ( r, c )) (colIndices grid)) (rowIndices grid)


getRow : Int -> Grid a -> Maybe (List a)
getRow y =
    Array.get y >> Maybe.map Array.toList


getCol : Int -> Grid a -> Maybe (List a)
getCol x =
    -- TODO: Can make this more efficient by modifying transpose to return the relevant column?
    transpose >> getRow x


setRow : Int -> List a -> Grid a -> Grid a
setRow r row g =
    if r < 0 || r >= Matrix.rowCount g || List.length row /= Matrix.colCount g then
        g

    else
        Array.set r (Array.fromList row) g


setCol : Int -> List a -> Grid a -> Grid a
setCol c col =
    transpose >> setRow c col >> transpose


flipRows : Grid a -> Grid a
flipRows grid =
    let
        numRows =
            Matrix.rowCount grid
    in
    Matrix.mapWithLocation (\( row, col ) val -> Matrix.get ( numRows - 1 - row, col ) grid |> Maybe.withDefault val) grid


flipCols : Grid a -> Grid a
flipCols grid =
    let
        numCols =
            Matrix.colCount grid
    in
    Matrix.mapWithLocation (\( row, col ) val -> Matrix.get ( row, numCols - 1 - col ) grid |> Maybe.withDefault val) grid


{-| Rotates grid 90 degress clockwise.
-}
rotate : Grid a -> Grid a
rotate =
    transpose >> flipCols


transpose : Grid a -> Grid a
transpose =
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
