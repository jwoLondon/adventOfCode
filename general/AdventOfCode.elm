{- Architecture for a reading a single string from an external file. -}


module AdventOfCode exposing (..)

import Dict
import Html exposing (Html, div, h1, text)
import Http


outFormat : a -> OutFormat
outFormat =
    toString >> text


type alias OutFormat =
    Html Msg


aoc : String -> (List String -> OutFormat) -> (List String -> OutFormat) -> Program Never Model Msg
aoc inFilename p1 p2 =
    Html.program
        { init = init inFilename p1 p2
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Utility functions


{-| Converts a function that takes a single string to generate output and provides
one that accepts a multi-iine input. Used for questions where input is on a single line.
-}
multiLineInput : (String -> OutFormat) -> (List String -> OutFormat)
multiLineInput f s =
    f (String.concat s)


{-| Compact string to integer for mre readable code.
-}
toInt : String -> Int
toInt =
    String.toInt >> Result.withDefault 0


{-| Generates all combinations of size k or smaller of an ordered list.
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


{-| From List.Extra: <http://package.elm-lang.org/packages/elm-community/list-extra/6.1.0/List-Extra>
Return the list of of all permutations of a list. The result is in lexicographic order.
permutations [1,2,3] == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
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


{-| From List.Extra: <http://package.elm-lang.org/packages/elm-community/list-extra/6.1.0/List-Extra>
Return all combinations in the form of (element, rest of the list).
select [1,2,3,4] == [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
-}
select : List a -> List ( a, List a )
select xs =
    case xs of
        [] ->
            []

        x :: xs ->
            ( x, xs ) :: List.map (\( y, ys ) -> ( y, x :: ys )) (select xs)


{-| Return all combinations in the form of (element, rest of the list) where element
is larger than all items in the rest of list.
select [1,2,3,4] == [(1,[]),(2,[1]),(3,[1,2]),(4,[1,2,3])]
-}
selectLargest : List comparable -> List ( comparable, List comparable )
selectLargest xs =
    case xs of
        [] ->
            []

        x :: xs ->
            ( x, List.filter (\y -> y < x) xs )
                :: List.map (\( y, ys ) -> ( y, x :: ys )) (selectLargest xs)


{-| Provides a list of all the factors of a given number.
-}
factors : Int -> List Int
factors n =
    let
        fac : Int -> Int -> List Int -> List Int
        fac n i facs =
            if i == 1 then
                1 :: n :: facs
            else if n % i == 0 then
                fac n (i - 1) (i :: (n // i) :: facs)
            else
                fac n (i - 1) facs

        upper =
            round (sqrt (toFloat n))
    in
    fac n upper []


{-| Transposes a list of lists, swappings rows for columns.
-}
transpose : List (List a) -> List (List a)
transpose ll =
    let
        heads =
            List.filterMap List.head ll

        tails =
            List.filterMap List.tail ll
    in
    if List.length heads == List.length ll then
        heads :: transpose tails
    else
        []


{-| Provides a binary representation of the given decimal. Represented as a list
of 1s and 0s with a minumum length determined by the padding parameter.
-}
decToBinary : Int -> List Int -> Int -> List Int
decToBinary padding bin dec =
    if dec == 0 then
        List.repeat (max 0 (padding - List.length bin)) 0 ++ bin
    else
        let
            bit =
                if dec % 2 == 0 then
                    0
                else
                    1
        in
        decToBinary padding (bit :: bin) (dec // 2)


{-| Provides a binary representation of the given hexidecimal number. Represented
as a list of 1s and 0s.
-}
hexToBinary : String -> List Int
hexToBinary hexStr =
    let
        hexLookup =
            List.map2 (,)
                ("0123456789abcdef" |> String.toList)
                (List.map (decToBinary 4 []) (List.range 0 15))
                |> Dict.fromList

        toBits hexChr =
            Dict.get hexChr hexLookup |> Maybe.withDefault []
    in
    hexStr
        |> String.toList
        |> List.foldl (\c digits -> digits ++ toBits c) []



-- MODEL


type alias Model =
    { input : List String
    , part1 : List String -> OutFormat
    , part2 : List String -> OutFormat
    }


init : String -> (List String -> Html Msg) -> (List String -> Html Msg) -> ( Model, Cmd Msg )
init filename part1 part2 =
    ( Model [] part1 part2
    , filename |> Http.send FileRead << Http.getString
    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        output =
            if model.input == [] then
                [ h1 [] [ text "Processing..." ] ]
            else
                [ h1 [] [ text "Part 1" ]
                , model.part1 model.input
                , h1 [] [ text "Part 2" ]
                , model.part2 model.input
                ]
    in
    div [] output



-- UPDATE (handles input file reading)


type Msg
    = FileRead (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileRead (Ok input) ->
            ( { model | input = String.lines input }, Cmd.none )

        FileRead (Err err) ->
            ( { model | input = "Error: " ++ toString err |> String.lines }, Cmd.none )
