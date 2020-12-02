---
follows: data/d01_2020.md

id: "litvis"
---

@import "../css/aoc.less"

# Day 1 (2020): Report Repair

Exploring the distribution of pairwise combinations of input numbers. Only one should intersect with the line $x+y=2020$. With my input (and others' ?), it included the smallest number in the input list (indicated as a red circle in the chart below).

```elm {l=hidden}
tupleFromList : List Int -> ( Int, Int )
tupleFromList xs =
    case xs of
        x1 :: x2 :: tl ->
            ( x1, x2 )

        _ ->
            ( 0, 0 )


pairs2 : List ( Int, Int )
pairs2 =
    AOC.combinations 2 puzzleInput |> List.map tupleFromList
```

```elm {v interactive}
pairChart : VL.Spec
pairChart =
    let
        data =
            VL.dataFromColumns []
                << VL.dataColumn "x" (VL.nums (pairs2 |> List.map (Tuple.first >> toFloat)))
                << VL.dataColumn "y" (VL.nums (pairs2 |> List.map (Tuple.second >> toFloat)))

        sel =
            VL.selection
                << VL.select "mySelection" VL.seInterval [ VL.seBindScales ]

        lineData =
            VL.dataFromColumns []
                << VL.dataColumn "x" (VL.nums [ 0, 2020 ])
                << VL.dataColumn "y" (VL.nums [ 2020, 0 ])

        enc =
            VL.encoding
                << VL.position VL.X [ VL.pName "x", VL.pQuant, VL.pScale [ VL.scDomain (VL.doNums [ 0, 2020 ]) ] ]
                << VL.position VL.Y [ VL.pName "y", VL.pQuant ]

        dotSpec =
            VL.asSpec [ sel [], data [], VL.point [] ]

        lineSpec =
            VL.asSpec [ lineData [], VL.line [ VL.maColor "black" ] ]

        trans =
            VL.transform
                << VL.filter (VL.fiExpr "datum.x + datum.y == 2020")

        intersectionSpec =
            VL.asSpec [ data [], trans [], VL.circle [ VL.maColor "red" ] ]
    in
    VL.toVegaLite [ VL.width 800, VL.height 800, enc [], VL.layer [ dotSpec, intersectionSpec, lineSpec ] ]
```
