{- Rudolph the Red-Nosed Reindeer is sick! His nose isn't shining very brightly,
   and he needs medicine.

   Red-Nosed Reindeer biology isn't similar to regular reindeer biology; Rudolph
   is going to need custom-made medicine. Unfortunately, Red-Nosed Reindeer chemistry
   isn't similar to regular reindeer chemistry, either.

   The North Pole is equipped with a Red-Nosed Reindeer nuclear fusion/fission plant,
   capable of constructing any Red-Nosed Reindeer molecule you need. It works by
   starting with some input molecule and then doing a series of replacements, one
   per step, until it has the right molecule.

   However, the machine has to be calibrated before it can be used. Calibration
   involves determining the number of molecules that can be generated in one step
   from a given starting point.

   For example, imagine a simpler machine that supports only the following replacements:

   H => HO
   H => OH
   O => HH

   Given the replacements above and starting with HOH, the following molecules could
   be generated:

   HOOH (via H => HO on the first H).
   HOHO (via H => HO on the second H).
   OHOH (via H => OH on the first H).
   HOOH (via H => OH on the second H).
   HHHH (via O => HH).

   So, in the example above, there are 4 distinct molecules (not five, because HOOH
   appears twice) after one replacement from HOH. Santa's favorite molecule, HOHOHO,
   can become 7 distinct molecules (over nine replacements: six from H, and three
   from O).

   The machine replaces without regard for the surrounding characters. For example,
   given the string H2O, the transition H => OO would result in OO2O.

   Your puzzle input describes all of the possible replacements and, at the bottom,
   the medicine molecule for which you need to calibrate the machine. How many distinct
   molecules can be created after all the different ways you can do one replacement
   on the medicine molecule?

   --- Part Two ---

   Now that the machine is calibrated, you're ready to begin molecule fabrication.

   Molecule fabrication always begins with just a single electron, e, and applying
   replacements one at a time, just like the ones during calibration.

   For example, suppose you have the following replacements:

   e => H
   e => O
   H => HO
   H => OH
   O => HH

   If you'd like to make HOH, you start with e, and then make the following replacements:

   e => O to get O
   O => HH to get HH
   H => OH (on the second H) to get HOH

   So, you could make HOH after 3 steps. Santa's favorite molecule, HOHOHO, can be
   made in 6 steps.

   How long will it take to make the medicine? Given the available replacements
   and the medicine molecule in your puzzle input, what is the fewest number of
   steps to go from e to the medicine molecule?
-}


module Main exposing (..)

import AdventOfCode exposing (Model, Msg, aoc, outFormat)
import Dict exposing (Dict)
import Regex
import Set exposing (Set)


type alias ReplacementRules =
    Dict String (List String)


type alias RevReplacementRules =
    List ( String, String )


type alias Selection =
    ( String, String, String )


medicine : String
medicine =
    "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF"


main : Program Never Model Msg
main =
    aoc "data/d19_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 input =
    calibrate medicine (parse input) |> List.length


part2 : List String -> Int
part2 input =
    let
        ( n, e ) =
            traceback (reverseLookup (parse input) []) medicine 0
    in
    n


calibrate : String -> ReplacementRules -> List String
calibrate molecule rules =
    let
        selections =
            split molecule |> selectAndSplit
    in
    generate selections rules []
        |> unique


generate : List Selection -> ReplacementRules -> List String -> List String
generate selections rules molecules =
    case selections of
        [] ->
            molecules

        selection :: tl ->
            generate tl rules (molecules ++ replacements selection rules)


replacements : Selection -> ReplacementRules -> List String
replacements ( left, atom, right ) rules =
    let
        newMolecules =
            Dict.get atom rules
                |> Maybe.withDefault []
    in
    List.map (\m -> left ++ m ++ right) newMolecules


{-| Provides a reverse lookup (molecule to parent) ordered by molecule length.
Lookup provided in the form of a list of (molecule,parent) tuples rather
than a Dict so it can be sorted.
-}
reverseLookup : Dict String (List String) -> List ( String, String ) -> List ( String, String )
reverseLookup dict revDict =
    let
        byLength ( k1, v1 ) ( k2, v2 ) =
            compare (String.length k1) (String.length k2)

        add key values revDict =
            case values of
                [] ->
                    revDict

                hd :: tl ->
                    add key tl (( hd, key ) :: revDict)
    in
    case Dict.toList dict of
        [] ->
            revDict |> List.sortWith byLength |> List.reverse

        ( key, values ) :: tl ->
            reverseLookup (Dict.fromList tl) (add key values revDict)


traceback : RevReplacementRules -> String -> Int -> ( Int, String )
traceback revRules compound n =
    if compound == "e" then
        ( n, "e" )
    else
        let
            ( m, newCompound ) =
                back revRules compound 0
        in
        if newCompound == compound then
            ( n + m, compound )
        else
            traceback revRules newCompound (n + m)


back : RevReplacementRules -> String -> Int -> ( Int, String )
back revRules compound n =
    case revRules of
        [] ->
            ( n, compound )

        ( k, v ) :: tl ->
            let
                ( m, newCompound ) =
                    parent k v compound
            in
            back tl newCompound (n + m)


parent : String -> String -> String -> ( Int, String )
parent molecule prevM compound =
    let
        m =
            Regex.find Regex.All (Regex.regex molecule) compound
                |> List.length

        newCompound =
            Regex.replace Regex.All (Regex.regex molecule) (\_ -> prevM) compound
    in
    ( m, newCompound )


selectAndSplit : List String -> List ( String, String, String )
selectAndSplit list =
    let
        select : List String -> List String -> List ( List String, String, List String )
        select left right =
            case right of
                [] ->
                    []

                hd :: tl ->
                    ( left, hd, tl ) :: List.map (\( l, x, r ) -> ( hd :: l, x, r )) (select left tl)
    in
    List.map (\( l, c, r ) -> ( String.concat l, c, String.concat r )) (select [] list)


{-| Remove duplicate values, keeping the first instance of each element which appears
more than once. From List.extra library
<http://package.elm-lang.org/packages/elm-community/list-extra/6.1.0/List-Extra>

    unique [0,1,1,0,1] == [0,1]

-}
unique : List comparable -> List comparable
unique list =
    let
        uniqueHelp f existing remaining =
            case remaining of
                [] ->
                    []

                first :: rest ->
                    let
                        computedFirst =
                            f first
                    in
                    if Set.member computedFirst existing then
                        uniqueHelp f existing rest
                    else
                        first :: uniqueHelp f (Set.insert computedFirst existing) rest
    in
    uniqueHelp identity Set.empty list


containsSubsets : ReplacementRules -> Bool
containsSubsets rules =
    let
        molecules =
            Dict.values rules
                |> List.concat

        totalSupersets allMol molecules n =
            case molecules of
                [] ->
                    n

                hd :: tl ->
                    totalSupersets allMol tl (n + numSupersets hd allMol)
    in
    totalSupersets molecules molecules 0 > List.length molecules


numSupersets : String -> List String -> Int
numSupersets molecule molecules =
    let
        submatch s1 s2 =
            if String.contains s1 s2 then
                1
            else
                0
    in
    List.map (submatch molecule) molecules
        |> List.sum


parse : List String -> ReplacementRules
parse input =
    List.foldl parseLine Dict.empty input


parseLine : String -> ReplacementRules -> ReplacementRules
parseLine text rules =
    let
        append : String -> Maybe (List String) -> Maybe (List String)
        append newV v =
            case v of
                Nothing ->
                    Just [ newV ]

                Just val ->
                    Just (newV :: val)

        matches text =
            text
                |> Regex.find (Regex.AtMost 1)
                    (Regex.regex "(\\w+) => (\\w+)")
                |> List.map .submatches
    in
    case matches text of
        [ [ Just m0, Just molecules ] ] ->
            Dict.update m0 (append molecules) rules

        _ ->
            rules


split : String -> List String
split molecules =
    -- TODO: Is there a better regex for splitting that does not generate empty list elements?
    Regex.split Regex.All (Regex.regex "([A-Z][a-z]*)") molecules
        |> List.filter (\s -> String.length s > 0)
