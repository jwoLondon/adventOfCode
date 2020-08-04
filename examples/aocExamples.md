---
elm:
  dependencies:
    folkertdev/elm-deque: latest
    fifth-postulate/priority-queue: latest
    elm/regex: latest

  source-directories:
    - ../src
    - ../src/dependencies

id: "litvis"
---

@import "../css/aoc.less"

```elm {l=hidden}
import Aoc as AOC
import BoundedDeque exposing (BoundedDeque)
import Deque exposing (Deque)
import Dict exposing (Dict)
import PriorityQueue
import Regex
```

# AoC Module Directory

Functions available in the Aoc module with simple working examples.

## Table of Contents

### 1. List Processing.

[scanl](#scanl), [neighbours](#neighbours), [circularNeighbours](#circularneighbours), [rotateList](#rotatelist), [makeCycle](#makecycle), [groupsOf](#groupsof), [dropWhile](#dropwhile), [takeWhile](#takewhile), [indexOf](#indexof), [setListAt](#setlistat), [splitAt](#splitat), [zip](#zip), [transpose](#transpose), [unique](#unique).

### 2. Parsing

[contains](#contains), [match](#match), [submatches](#submatches), [split](#split), [replace](#replace), [replaceFn](#replacefn), [toInt](#toint).

### 3. Priority Queue.

[creating a priority queue](#creating-a-priority-queue)

### 4. Double Ended Queue (Deque).

[mapHeadDeque](#mapheaddeque), [rotatedeque](#rotatedeque).

### 5. Combinatorics

[combinations](#combinations), [pairwiseCombinations](#pairwisecombinations), [gridLocations](#gridlocations), [permutations](#permutations), [select](#select), [selectLargest](#selectlargest), [selectSplit](#selectsplit).

### 6. Number Theory

[lowestCommonMultiple](#lowestcommonmultiple), [factors](#factors), [highestCommonFactor](#highestcommonfactor)

### 7. Number Conversion

[decToBinary](#dectobinary), [decToHex](#dectohex), [hexToBinary](#hextobinary)

### 8. Frequency Distributions

[addToFreqTable](#addtofreqtable), [addNToFreqTable](#addntofreqtable), [updateInsert](#updateinsert), [mode](#mode), [modeCount](#modecount)

### 9. Cycle Detection

[sequenceCycle](#sequencecycle).

### 10. Grid.

[gInit](#ginit), [gFromList](#gfromlist), [gFromLists](#gfromlists), [gColCount](#gcolcount), [gRowCount](#growcount), [gGet](#gget), [gGetCol](#ggetcol), [gGetRow](#ggetrow), [gSet](#gset), [gSetCol](#gsetcol), [gSetRow](#gsetrow), [gMap](#gmap), [gMapWithLocation](#gmapwithlocation), [gTranspose](#gtranspose), [gToList](#gtolist), [gToLists](#gtolists).

### 11. 3-tuples

[triplet](#triplet), [tripletFromList](#tripletfromlist), [tripletFirst](#tripletfirst), [tripletSecond](#tripletsecond), [tripletThird](#trioletthird), [mapTriplet](#maptriplet), [zip3](#zip3), [unzip3](#unzip3)

### 12. Functional Utilities

[flip](#flip), [iterate](#iterate), [uncurry](#uncurry), [curry](#curry).

---

## 1. List Processing

Functions for manipulating 1d lists of items.

### scanl

Fold over a list with a given accumulating function, storing each step in a list.

```elm {l r siding}
example : List Int
example =
    AOC.scanl (+) 1 (List.range 2 10)
```

### neighbours

Create a list of adjacent neighbour tuples from a list. Will be one shorter than the original list.

```elm {l r siding}
example : List ( Int, Int )
example =
    AOC.neighbours (List.range 0 4)
```

### circularNeighbours

Create a circular list of adjacent neighbour tuples from a list where the first item is the previous (circular) list item.

```elm {l r siding}
example : List ( Int, Int )
example =
    AOC.circularNeighbours (List.range 0 4)
```

### rotateList

Move the last item in the list to the front.

```elm {l r siding}
example : List Int
example =
    AOC.rotateList (List.range 0 5)
```

### makeCycle

For a list of lists, add the first element of each list to its end, making a list of closed cycles.

```elm {l r siding}
example : List (List ( Int, String ))
example =
    AOC.makeCycle
        [ [ ( 0, "a" ), ( 0, "b" ), ( 0, "c" ) ]
        , [ ( 1, "a" ), ( 1, "b" ), ( 1, "c" ) ]
        ]
```

### groupsOf

Split a list into groups of a given size. If there are not enough elements to completely fill the last group, it will not be included.

```elm {l r siding}
example : List (List Int)
example =
    AOC.groupsOf 3 (List.range 1 10)
```

### dropWhile

Drop items from the front of a list until the given predicate fails.

```elm {l r siding}
example : List Int
example =
    AOC.dropWhile (\n -> n <= 0) [ -6, -3, -1, 0, 4, -5, 10 ]
```

### takeWhile

Take items from the front of a list until the given predicate fails.

```elm {l r siding}
example : List Int
example =
    AOC.takeWhile (\n -> n < 0) [ -6, -3, -1, 0, 4, -5, 10 ]
```

### indexOf

Find the index of the first occurrence of a value in a list or -1 if not found.

```elm {l r siding}
example : Int
example =
    AOC.indexOf "c" [ "a", "b", "c", "d", "e", "f" ]
```

### setListAt

Replace the value at the given index with a new one.

```elm {l r siding}
example : List String
example =
    AOC.setListAt 4 "E" [ "a", "b", "c", "d", "e", "f" ]
```

### splitAt

Split a list into two at the given index position.

```elm {l r siding}
example : ( List String, List String )
example =
    AOC.splitAt 4 [ "a", "b", "c", "d", "e", "f" ]
```

### zip

Join two lists together as a single list of tuples.

```elm {l r siding}
example : List ( Int, String )
example =
    AOC.zip [ 1, 2, 3, 4 ] [ "a", "b", "c", "d" ]
```

### transpose

Transpose a list of lists, swapping rows and columns.

```elm {l r siding}
example : List (List ( Int, String ))
example =
    AOC.transpose
        [ [ ( 0, "a" ), ( 0, "b" ), ( 0, "c" ) ]
        , [ ( 1, "a" ), ( 1, "b" ), ( 1, "c" ) ]
        ]
```

### unique

Create a list of unique values preserving the original list order of first occurrence.

```elm {l r siding}
example : List Int
example =
    AOC.unique [ 4, 4, 4, 4, 2, 1, 2, 8, 8, 8, 8, 8, 8, 4 ]
```

---

## 2. Parsing

### contains

Indicate whether the text in the second parameter contains any matches given by the given regular expression (first parameter).

```elm {l  r siding}
example : List Bool
example =
    List.map (AOC.contains "^.*\\b([Oo]ne|three)\\b.*$") [ "One cat", "two fish", "and three dogs" ]
```

### match

List of matches determined by the regex (first parameter) found in the text of the second parameter This version is useful for simpler regular expressions that do not group into sub-matches.

```elm {l  r siding}
example : List String
example =
    AOC.match "^.*[Oo]ne|three.*$" "One cat, two fish and three dogs."
```

### submatches

Given a regex containing groups (first parameter), provide a list of sub (grouped) matches found in the text of the second parameter. Allows regex groups to be identified and where matched, will be `Just` a match or `Nothing` if the group does not match.

```elm {l  r siding}
example : List String
example =
    AOC.submatches "^.*?\\b([Oo]ne|two|three)\\b.*$" "One cat, two fish and three dogs."
        |> List.filterMap identity
```

### split

Split a string (second parameter) by patterns identified by a regex (first parameter).

```elm {l  r siding}
example : List String
example =
    AOC.split "\\s*,\\s*|\\s*and\\s*" "One   cat,  two fish  and   three dogs."
```

### replace

Search using a given regex (first parameter) replacing matches with the second parameter applying it to the text of the third parameter.

```elm {l  r siding}
example : String
example =
    "One cat, two fish and three dogs."
        |> AOC.replace "([Oo]ne)" "1"
        |> AOC.replace "([Tt]wo)" "2"
        |> AOC.replace "([Tt]hree)" "3"
```

### replaceFn

Replace with a given match function. It is syntactic sugar for [Regex.replace](https://package.elm-lang.org/packages/elm/regex/latest/Regex#replace).

```elm {l  r siding}
example : String
example =
    "888776"
        |> AOC.replaceFn "(\\d)\\1*"
            (\m -> (String.length m.match |> String.fromInt) ++ String.left 1 m.match)
```

### toInt

Unsafe, but compact string to integer conversion for more readable code.

```elm {l r siding}
example : Int
example =
    AOC.toInt "345"
```

---

## 3. Priority Queue

Functions for manipulating a priority queue (queue structure with a priority function for maintaining items in user-defined order).

### creating a priority queue

```elm {l r siding}
example : List String
example =
    "The cat sat on the mat while deciding whether or not to go to sleep."
        |> String.words
        |> PriorityQueue.fromList (negate << String.length)
        >> PriorityQueue.toList
```

---

## 4. Double Ended Queue (Deque).

Functions for manipulating double-ended queues. Useful for efficient shifting of circular lists.

### Create a deque

Create a deque from a list.

```elm {l r}
myDeque : Deque Int
myDeque =
    Deque.fromList (List.range 1 7)
```

Create a bounded deque (i.e. with a maximum size beyond which items are discarded from tail when new items added) from a list.

```elm {l r siding}
example : List Int
example =
    BoundedDeque.fromList 5 (List.range 1 999)
        |> BoundedDeque.toList
```

### mapHeadDeque

Change the head of a deque.

```elm {l r siding}
example : List Int
example =
    AOC.mapHeadDeque ((+) 10) myDeque
        |> Deque.toList
```

### rotateDeque

Rotate all values in a deque two places forward.

```elm {l r siding}
example : List Int
example =
    AOC.rotateDeque 2 myDeque
        |> Deque.toList
```

---

## 5. Combinatorics

Functions for combining and selecting combinations and permutations of list elements.

### combinations

Generate all combinations of size _k_ or smaller of an ordered list.

```elm {l r siding}
example : List (List Int)
example =
    AOC.combinations 3 (List.range 1 4)
```

### pairwiseCombinations

Generate all combinations of size _k_ or smaller of an ordered list.

```elm {l r siding}
example : List ( Int, Int )
example =
    AOC.pairwiseCombinations (List.range 1 4)
```

### gridLocations

Generate a list of coordinate pairs between the given minimum and maximum positions inclusive.

```elm {l r siding}
example : List ( Int, Int )
example =
    AOC.gridLocations ( 10, 10 ) ( 12, 13 )
```

### permutations

Generate all permutations of items in a list.

```elm {l r siding}
example : List (List Int)
example =
    AOC.permutations (List.range 1 3)
```

### select

Provide all combinations in the form of (_e_, _rest of the list_) from a list of items.

```elm {l r siding}
example : List ( Int, List Int )
example =
    AOC.select (List.range 1 4)
```

### selectLargest

Provide all combinations in the form of (_e_, _rest of the list_) where _e_ is larger than all items in the rest of list from a list of items.

```elm {l r siding}
example : List ( Int, List Int )
example =
    AOC.selectLargest (List.range 1 4)
```

### selectSplit

Provide all combinations in the form of (_elements before_, _element_, _elements after_) from a list of items.

```elm {l r siding}
example : List ( List Int, Int, List Int )
example =
    AOC.selectSplit (List.range 1 4)
```

---

## 6. Number Theory

Factors and multiples.

### lowestCommonMultiple

Smallest number that is divisible by both the given numbers.

```elm {l r siding}
example : Int
example =
    AOC.lowestCommonMultiple 4 6
```

### factors

All the factors (divisors with no remainder) of a given number. A prime will have factors of just 1 and the prime itself.

```elm {l r siding}
example : List Int
example =
    AOC.factors 12
```

### highestCommonFactor

Highest common factor (HCF) of the two given numbers. Note that for convenience, this also returns a value when both numbers are 0 (1), when one is zero (the non-zero number) and when values are negative.

```elm {l r siding}
example : Int
example =
    AOC.highestCommonFactor 12 16
```

---

## 7. Number Conversion

### decToBinary

Provide a binary representation of the given decimal. Represented as a list of 1s and 0s with a minimum length determined by the first parameter.

```elm {l r siding}
example : String
example =
    AOC.decToBinary 8 25
        |> List.map String.fromInt
        |> String.concat
```

### decToHex

Provide a hexadecimal representation of the given decimal, where the first parameter is the number of digits to pad if necessary.

```elm {l r siding}
example : String
example =
    AOC.decToHex 4 25
```

### hexToBinary

Provide a binary representation of the given hexadecimal number. Represented as a list of 1s and 0s.

```elm {l r siding}
example : String
example =
    AOC.hexToBinary "e3d7"
        |> List.map String.fromInt
        |> String.concat
```

---

## 8. Frequency Distributions

### addToFreqTable

Accumulate frequencies in a frequency table one by one.

```elm {l r siding}
example : Dict String Int
example =
    List.foldl AOC.addToFreqTable Dict.empty [ "fish", "cat", "dog", "cat", "dog", "cat" ]
```

### addNToFreqTable

Accumulate frequencies in a frequency table.

```elm {l r siding}
example : Dict String Int
example =
    Dict.empty
        |> AOC.addNToFreqTable "cat" 49
        |> AOC.addNToFreqTable "fish" 10
        |> AOC.addNToFreqTable "dog" 50
        |> AOC.addNToFreqTable "cat" 50
```

### updateInsert

Add an item to a dictionary if it does not exist, but replace an existing one if it does.

```elm {l r siding}
example : Dict String Int
example =
    Dict.empty
        |> AOC.updateInsert "cat" 49
        |> AOC.updateInsert "fish" 10
        |> AOC.updateInsert "dog" 50
        |> AOC.updateInsert "cat" 50
```

### mode

Commonest value in a list.

```elm {l r siding}
example : Maybe String
example =
    AOC.mode [ "cat", "dog", "cat", "fish" ]
```

### modeCount

Frequency of the commonest value in a list.

```elm {l r siding}
example : Maybe Int
example =
    AOC.modeCount [ "cat", "dog", "cat", "fish" ]
```

---

## 9. Cycle Detection

Given a maximum sequence length, a start value and a function that generates the next value in a sequence, return the length of the cycle and the zero-indexed position in the sequence of the first element of the cycle. If there is no cycle
detected less than the maximum sequence length, will return `Nothing`.

```elm {l r siding}
mySequence : List Int
mySequence =
    AOC.scanl (\_ x -> x ^ 2 + 1 |> modBy 255) 3 (List.range 1 10)


example : Maybe ( Int, Int )
example =
    AOC.sequenceCycle 10 3 (\x -> x ^ 2 + 1 |> modBy 255)
```

---

## 10. Grid Manipulation

Functions for manipulating fixed size 2d grids of items.

### 10.1 Building

### gInit

Initialise a grid's dimensions and values.

```elm {l r siding}
example : List (List String)
example =
    AOC.gInit 3 4 "a"
        |> AOC.gToLists
```

### gFromList

Initialise a grid with a given number of columns from a given list of values.

```elm {l r siding}
example : List (List String)
example =
    AOC.gFromList 4 (List.repeat 12 "a")
        |> AOC.gToLists
```

### gFromLists

Initialise a grid from a given list of lists.

```elm {l r siding}
example : List (List String)
example =
    AOC.gFromLists (List.repeat 3 (List.repeat 4 "a"))
        |> AOC.gToLists
```

### 10.2 Query

```elm {l=hidden}
g : AOC.Grid String
g =
    AOC.gInit 3 4 "a"
```

### gColCount

Number of columns in a grid.

```elm {l r siding}
example : Int
example =
    AOC.gColCount g
```

### gRowCount

Number of rows in a grid.

```elm {l r siding}
example : Int
example =
    AOC.gRowCount g
```

### gGet

Grid value at a given position.

```elm {l r siding}
example : Maybe String
example =
    AOC.gGet ( 2, 3 ) g
```

### gGetCol

Grid values in the given column position.

```elm {l r siding}
example : Maybe (List String)
example =
    AOC.gGetCol 2 g
```

### gGetRow

Grid values in the given row position.

```elm {l r siding}
example : Maybe (List String)
example =
    AOC.gGetRow 2 g
```

### 10.3 Editing

### gGet

Set grid value at a given position.

```elm {l r siding}
example : List (List String)
example =
    AOC.gSet ( 1, 3 ) "B" g
        |> AOC.gToLists
```

### gSetCol

Set values at a given column in a grid.

```elm {l r siding}
example : List (List String)
example =
    AOC.gSetCol 2 [ "B", "B", "B" ] g
        |> AOC.gToLists
```

### gSetRow

Set values at a given row in a grid.

```elm {l r siding}
example : List (List String)
example =
    AOC.gSetRow 1 [ "B", "B", "B", "B" ] g
        |> AOC.gToLists
```

### gMap

Map all the values in a grid with the given function.

```elm {l r siding}
example : List (List String)
example =
    AOC.gMap String.toUpper g
        |> AOC.gToLists
```

### gMapWithLocation

Map all the values in a grid with the given function that can use row and column coordinates.

```elm {l r siding}
example : List (List ( String, String ))
example =
    AOC.gMapWithLocation (\( r, c ) s -> ( String.fromInt r ++ s, String.fromInt c ++ s )) g
        |> AOC.gToLists
```

### gTranspose

Transpose rows and columns in a grid.

```elm {l r siding}
example : List (List String)
example =
    AOC.gTranspose g
        |> AOC.gToLists
```

### 10.4 Output

### gToList

Convert grid into a 1d list.

```elm {l r siding}
example : List String
example =
    AOC.gToList g
```

### gToLists

Convert grid into a 1d list.

```elm {l r siding}
example : List (List String)
example =
    AOC.gToLists g
```

---

## 11. 3-Tuples

Elm has functions for creating and transforming tuples with two parameters, but not for three. These functions provide the extra support for 3-tuples:

### triplet

Create a 3-tuple from three parameters.

```elm {l r}
myTriplet : ( String, Int, Bool )
myTriplet =
    AOC.triplet "Cat" 8 True
```

### tripletFromList

Create a 3-tuple from the first three items in a list.

```elm {l r siding}
example : Maybe ( Int, Int, Int )
example =
    AOC.tripletFromList (List.range 1 10)
```

### tripletFirst

Extract first item from a 3-tuple:

```elm {l r siding}
example : String
example =
    AOC.tripletFirst myTriplet
```

### tripletSecond

Extract second item from a 3-tuple:

```elm {l r siding}
example : Int
example =
    AOC.tripletSecond myTriplet
```

### tripletThird

Extract first item from a 3-tuple:

```elm {l r siding}
example : Bool
example =
    AOC.tripletThird myTriplet
```

### mapTriplet

Transform all three values of a 3-tuple.

```elm {l r siding}
example : ( String, Int, Bool )
example =
    AOC.mapTriplet String.toUpper ((+) 1) not myTriplet
```

### zip3

Zip three lists together into a list of 3-tuples.

```elm {l r siding}
example : List ( Int, String, Bool )
example =
    AOC.zip3 [ 1, 2 ] [ "a", "b" ] [ True, False ]
```

### unzip3

Unzip a list of 3-tuples into a 3-tuple of lists.

```elm {l r siding}
example : ( List Int, List Int, List Int )
example =
    AOC.unzip3 [ ( 1, 2, 3 ), ( 4, 5, 6 ) ]
```

---

## 12. Functional Utilities

### flip

Flip the order of parameters in a function. Useful for point-free notation.

```elm {l r siding}
example : List Int
example =
    List.map (AOC.flip modBy 10) (List.range 1 10)
```

### iterate

Convenience function for repeating a function a fixed number of times.

```elm {l r siding}
example : Int
example =
    AOC.iterate 10 0 ((+) 2)
```

### uncurry

Combine two arguments into a single pair.

```elm {l r siding}
example : Int
example =
    AOC.uncurry (+) ( 3, 4 )
```

### curry

Split paired arguments into two separate ones.

```elm {l r siding}
example : Int
example =
    AOC.curry Tuple.first 3 4
```

### ifelse

Compact ternary function.