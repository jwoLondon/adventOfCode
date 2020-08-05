module KnotHash exposing
    ( OffsetList
    , knot
    , knotHash
    , rotateByN
    )

import Bitwise


{-| Represent state of a knothash twist.
-}
type alias OffsetList =
    { position : Int
    , skipSize : Int
    , data : List Int
    }


{-| Represent the twist lengths to apply to the data.
-}
type alias TwistLengths =
    List Int


{-| Rotate items in a list by n positions
-}
rotateByN : Int -> List a -> List a
rotateByN n xs =
    let
        len =
            List.length xs
    in
    List.drop (len - modBy len -n) xs ++ List.take (len - modBy len -n) xs


{-| Twist a given set of twist points in a list of numbers by the given set of
twist lengths.
-}
knot : OffsetList -> TwistLengths -> OffsetList
knot offsetList twistLengths =
    let
        reversePart n xs =
            (List.take n xs |> List.reverse) ++ List.drop n xs

        pinchAndTwist twistLength ol =
            OffsetList (modBy (List.length ol.data) (ol.position + twistLength + ol.skipSize))
                (ol.skipSize + 1)
                ((reversePart twistLength >> rotateByN (twistLength + ol.skipSize)) ol.data)
    in
    List.foldl pinchAndTwist offsetList twistLengths


{-| Generate a 64 bit hex string knot hash of the given string.
-}
knotHash : String -> String
knotHash inStr =
    let
        flip fn argB argA =
            fn argA argB

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

        kHash dense sparse =
            if sparse == [] then
                dense

            else
                let
                    blockHex =
                        sparse
                            |> List.take 16
                            |> List.foldl Bitwise.xor 0
                            |> decToHex 2
                in
                kHash (dense ++ blockHex) (List.drop 16 sparse)

        multiKnot repeats offsetList twistLengths =
            List.range 1 repeats
                |> List.foldl (\_ ofl -> knot ofl twistLengths) offsetList

        ol =
            inStr
                |> String.toList
                |> List.map Char.toCode
                |> flip (++) [ 17, 31, 73, 47, 23 ]
                |> multiKnot 64 (OffsetList 0 0 (List.range 0 255))
    in
    rotateByN -ol.position ol.data
        |> kHash ""
