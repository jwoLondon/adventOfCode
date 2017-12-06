module Grid exposing (..)

import Array exposing (Array)
import Matrix exposing (Matrix)


{- This version wraps the Matrix module and adds row and column functions. -}


type alias Location =
    Matrix.Location


type alias Grid a =
    Matrix.Matrix a


init : Int -> Int -> a -> Grid a
init rowCount colCount =
    always >> Matrix.matrix rowCount colCount


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


colCount : Grid a -> Int
colCount =
    Matrix.colCount


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
