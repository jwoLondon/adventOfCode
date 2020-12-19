---
id: "litvis"

elm:
  dependencies:
    folkertdev/elm-deque: latest
    fifth-postulate/priority-queue: latest
    avh4/elm-fifo: latest
    elm/regex: latest
    elm/parser: latest
    gicentre/elm-vegalite: latest
    gicentre/elm-vega: latest
    drathier/elm-graph: latest
    krisajenkins/elm-astar: latest
    lynn/elm-arithmetic: latest
    elm-community/list-extra: latest
    cmditch/elm-bigint: latest
    rtfeldman/elm-hex: latest

  source-directories:
    - src
    - src/dependencies

narrative-schemas:
  - schemas/aoc
---

@import "css/aoc.less"

# Advent of Code utilities

This document does nothing more than set up the various package dependencies used by the Advent of Code solutions. Most are contained within the module Aoc, found in the src folder.

```elm {l=hidden}
import Aoc as AOC
import Arithmetic
import Array exposing (Array)
import BigInt exposing (BigInt)
import Bitwise
import BoundedDeque exposing (BoundedDeque)
import Deque exposing (Deque)
import Dict exposing (Dict)
import Fifo exposing (Fifo)
import Graph exposing (Graph)
import Graph.Pair
import Hex
import Json.Decode as JD
import KnotHash as KH
import List.Extra
import MD5Fast as MD5
import Parser exposing ((|.), (|=), Parser)
import PriorityQueue
import Regex
import Set exposing (Set)
import Vega as V
import VegaLite as VL
```
