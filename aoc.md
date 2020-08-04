---
id: "litvis"

elm:
  dependencies:
    folkertdev/elm-deque: latest
    fifth-postulate/priority-queue: latest
    elm/regex: latest

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
import BoundedDeque exposing (BoundedDeque)
import Deque exposing (Deque)
import Dict exposing (Dict)
import PriorityQueue
import Regex
```
