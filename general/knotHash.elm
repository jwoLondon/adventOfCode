module KnotHash exposing (knot, knotHash)

{- For generating 'knot hashes'.
   See http://adventofcode.com/2017/day/10 for details.
-}

import Bitwise
import Char


type alias OffsetList =
    ( Int, Int, List Int )


type alias TwistLengths =
    List Int


{-| Generates a 64 bit hex string knot hash of the given string
-}
knotHash : String -> String
knotHash inStr =
    let
        kHash dense sparse =
            if sparse == [] then
                dense
            else
                let
                    blockHex =
                        sparse
                            |> List.take 16
                            |> List.foldl Bitwise.xor 0
                            |> toHex
                in
                kHash (dense ++ blockHex) (List.drop 16 sparse)

        ( pos, _, numList ) =
            inStr
                |> String.toList
                |> List.map Char.toCode
                |> flip (++) [ 17, 31, 73, 47, 23 ]
                |> multiKnot 64 ( 0, 0, List.range 0 255 )
    in
    rotate -pos numList
        |> kHash ""


{-| Twists a given set of twist points in a list of numbers by the given set
of twist lengths
-}
knot : OffsetList -> TwistLengths -> OffsetList
knot offsetList twistLengths =
    let
        pinchAndTwist twistLength ( pos, skipSize, xs ) =
            ( (pos + twistLength + skipSize) % List.length xs
            , skipSize + 1
            , (reversePart twistLength >> rotate (twistLength + skipSize)) xs
            )
    in
    List.foldl pinchAndTwist offsetList twistLengths



-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Private functions


multiKnot : Int -> OffsetList -> TwistLengths -> OffsetList
multiKnot repeats offsetList twistLengths =
    List.range 1 repeats
        |> List.foldl (\_ ol -> knot ol twistLengths) offsetList


reversePart : Int -> List a -> List a
reversePart n xs =
    (List.take n xs |> List.reverse) ++ List.drop n xs


rotate : Int -> List a -> List a
rotate n xs =
    let
        len =
            List.length xs
    in
    List.drop (len - (-n % len)) xs ++ List.take (len - (-n % len)) xs


toHex : Int -> String
toHex n =
    let
        hexChr n =
            [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' ]
                |> List.drop n
                |> List.head
                |> Maybe.withDefault 'f'
    in
    [ hexChr (Bitwise.and (Bitwise.shiftRightBy 4 n) 0x0F), hexChr (Bitwise.and n 0x0F) ]
        |> String.fromList
