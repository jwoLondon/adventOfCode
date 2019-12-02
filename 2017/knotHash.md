---
follows: ../aoc.md
id: "litvis"
---

@import "../css/aoc.less"

# Knot Hash Utilities

To help keep the code understandable, we can define a record representing state of the twist and an alias representing the twist lengths to apply to the data:

```elm {l}
type alias OffsetList =
    { position : Int
    , skipSize : Int
    , data : List Int
    }


type alias TwistLengths =
    List Int
```

We need to be able to rotate the circular list by a given amount:

```elm {l}
rotate : Int -> List a -> List a
rotate n xs =
    let
        len =
            List.length xs
    in
    List.drop (len - modBy len -n) xs ++ List.take (len - modBy len -n) xs
```

## Knotting

Twists a given set of twist points in a list of numbers by the given set of twist lengths

```elm {l}
knot : OffsetList -> TwistLengths -> OffsetList
knot offsetList twistLengths =
    let
        reversePart n xs =
            (List.take n xs |> List.reverse) ++ List.drop n xs

        pinchAndTwist twistLength ol =
            OffsetList (modBy (List.length ol.data) (ol.position + twistLength + ol.skipSize))
                (ol.skipSize + 1)
                ((reversePart twistLength >> rotate (twistLength + ol.skipSize)) ol.data)
    in
    List.foldl pinchAndTwist offsetList twistLengths
```

## Hash Generation

Generates a 64 bit hex string knot hash of the given string

```elm {l}
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
    rotate -ol.position ol.data
        |> kHash ""
```
