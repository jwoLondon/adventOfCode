{- Because your neighbors keep defeating you in the holiday house decorating contest
   year after year, you've decided to deploy one million lights in a 1000x1000 grid.

   Furthermore, because you've been especially nice this year, Santa has mailed
   you instructions on how to display the ideal lighting configuration.

   Lights in your grid are numbered from 0 to 999 in each direction; the lights
   at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include
   whether to turn on, turn off, or toggle various inclusive ranges given as
   coordinate pairs. Each coordinate pair represents opposite corners of a rectangle,
   inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights
   in a 3x3 square. The lights all start turned off.

   To defeat your neighbors this year, all you have to do is set up your lights
   by doing the instructions Santa sent you in order.

   For example:

   * turn on 0,0 through 999,999 would turn on (or leave on) every light.
   * toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning
     off the ones that were on, and turning on the ones that were off.
   * turn off 499,499 through 500,500 would turn off (or leave off) the middle
     four lights.

   After following the instructions, how many lights are lit?

   --- Part Two ---

   You just finish implementing your winning light pattern when you realize you
   mistranslated Santa's message from Ancient Nordic Elvish.

   The light grid you bought actually has individual brightness controls; each
   light can have a brightness of zero or more. The lights all start at zero.

   The phrase turn on actually means that you should increase the brightness of
   those lights by 1.

   The phrase turn off actually means that you should decrease the brightness of
   those lights by 1, to a minimum of zero.

   The phrase toggle actually means that you should increase the brightness of
   those lights by 2.

   What is the total brightness of all lights combined after following Santa's
   instructions?

   For example:

   * turn on 0,0 through 0,0 would increase the total brightness by 1.
   * toggle 0,0 through 999,999 would increase the total brightness by 2000000.
-}


module D06_2015 exposing (Instruction, Lights, Location, getLocations, gridSize, lightAt, main, parse, part1, part2, setLight, switchLights, toggle1, toggle2, tuple, turnOff1, turnOff2, turnOn1, turnOn2)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, toInt)
import Array exposing (Array)


main : Program () Model Msg
main =
    aoc "../data/d06_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 instructions =
    let
        lights =
            List.foldl (switchLights True)
                (Array.repeat (gridSize * gridSize) 0)
                (List.map parse instructions)
    in
    Array.toList lights |> List.sum


part2 : List String -> Int
part2 instructions =
    let
        lights =
            List.foldl (switchLights False)
                (Array.repeat (gridSize * gridSize) 0)
                (List.map parse instructions)
    in
    Array.toList lights |> List.sum


type alias Instruction =
    { switch : String
    , tl : ( Int, Int )
    , br : ( Int, Int )
    }


type alias Location =
    ( Int, Int )


type alias Lights =
    Array Int


gridSize : Int
gridSize =
    1000


switchLights : Bool -> Instruction -> Lights -> Lights
switchLights isPart1 instruction lights =
    let
        locations =
            getLocations instruction.tl instruction.br instruction.tl []
    in
    case instruction.switch of
        "turn on" ->
            if isPart1 then
                List.foldl turnOn1 lights locations

            else
                List.foldl turnOn2 lights locations

        "turn off" ->
            if isPart1 then
                List.foldl turnOff1 lights locations

            else
                List.foldl turnOff2 lights locations

        "toggle" ->
            if isPart1 then
                List.foldl toggle1 lights locations

            else
                List.foldl toggle2 lights locations

        _ ->
            lights


parse : String -> Instruction
parse instructText =
    let
        words =
            String.words instructText
    in
    case words of
        [ "turn", state, tl, "through", br ] ->
            Instruction ("turn " ++ state) (tuple tl) (tuple br)

        [ "toggle", tl, "through", br ] ->
            Instruction "toggle" (tuple tl) (tuple br)

        _ ->
            Instruction "" ( 0, 0 ) ( 0, 0 )


tuple : String -> ( Int, Int )
tuple tupText =
    case String.split "," tupText of
        [ a, b ] ->
            ( toInt a, toInt b )

        _ ->
            ( 0, 0 )


lightAt : Location -> Lights -> Int
lightAt ( x, y ) =
    Array.get (y * gridSize + x) >> Maybe.withDefault 0


setLight : Location -> Int -> Lights -> Lights
setLight ( x, y ) =
    Array.set (y * gridSize + x)


turnOn1 : Location -> Lights -> Lights
turnOn1 location =
    setLight location 1


turnOff1 : Location -> Lights -> Lights
turnOff1 location =
    setLight location 0


toggle1 : Location -> Lights -> Lights
toggle1 location lights =
    if lightAt location lights == 1 then
        setLight location 0 lights

    else
        setLight location 1 lights


turnOn2 : Location -> Lights -> Lights
turnOn2 location lights =
    setLight location (lightAt location lights + 1) lights


turnOff2 : Location -> Lights -> Lights
turnOff2 location lights =
    setLight location (max 0 (lightAt location lights - 1)) lights


toggle2 : Location -> Lights -> Lights
toggle2 location lights =
    setLight location (lightAt location lights + 2) lights


getLocations : Location -> Location -> Location -> List Location -> List Location
getLocations ( left, top ) ( right, bottom ) ( x, y ) oldLocations =
    let
        locations =
            ( x, y ) :: oldLocations
    in
    if ( x, y ) == ( right, bottom ) then
        locations

    else if x == right then
        getLocations ( left, top ) ( right, bottom ) ( left, y + 1 ) locations

    else
        getLocations ( left, top ) ( right, bottom ) ( x + 1, y ) locations
