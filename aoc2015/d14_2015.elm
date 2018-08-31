{- This year is the Reindeer Olympics! Reindeer can fly at high speeds, but must
   rest occasionally to recover their energy. Santa would like to know which of
   his reindeer is fastest, and so he has them race.

   Reindeer can only either be flying (always at their top speed) or resting (not
   moving at all), and always spend whole seconds in either state.

   For example, suppose you have the following Reindeer:

   Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
   Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.

   After one second, Comet has gone 14 km, while Dancer has gone 16 km. After ten
   seconds, Comet has gone 140 km, while Dancer has gone 160 km. On the eleventh
   second, Comet begins resting (staying at 140 km), and Dancer continues on for
   a total distance of 176 km. On the 12th second, both reindeer are resting. They
   continue to rest until the 138th second, when Comet flies for another ten seconds.
   On the 174th second, Dancer flies for another 11 seconds.

   In this example, after the 1000th second, both reindeer are resting, and Comet
   is in the lead at 1120 km (poor Dancer has only gotten 1056 km by that point).
   So, in this situation, Comet would win (if the race ended at 1000 seconds).

   Given the descriptions of each reindeer (in your puzzle input), after exactly
   2503 seconds, what distance has the winning reindeer traveled?

   --- Part Two ---

   Seeing how reindeer move in bursts, Santa decides he's not pleased with the old
   scoring system.

   Instead, at the end of each second, he awards one point to the reindeer currently
   in the lead. (If there are multiple reindeer tied for the lead, they each get
   one point.) He keeps the traditional 2503 second time limit, of course, as doing
   otherwise would be entirely ridiculous.

   Given the example reindeer from above, after the first second, Dancer is in the
   lead and gets one point. He stays in the lead until several seconds into Comet's
   second burst: after the 140th second, Comet pulls into the lead and gets his
   first point. Of course, since Dancer had been in the lead for the 139 seconds
   before that, he has accumulated 139 points by the 140th second.

   After the 1000th second, Dancer has accumulated 689 points, while poor Comet,
   our old champion, only has 312. So, with the new scoring system, Dancer would
   win (if the race ended at 1000 seconds).

   Again given the descriptions of each reindeer (in your puzzle input), after
   exactly 2503 seconds, how many points does the winning reindeer have?
-}


module D14_2015 exposing (Reindeer, distanceAfter, main, parse, parseLine, part1, part2, pointsAfter, pointsAt)

import AdventOfCode exposing (Model, Msg, aoc, outFormat, submatches, toInt)


type alias Reindeer =
    { speed : Int
    , duration : Int
    , rest : Int
    }


main : Program () Model Msg
main =
    aoc "data/d14_2015.txt"
        (part1 >> outFormat)
        (part2 >> outFormat)


part1 : List String -> Int
part1 input =
    let
        distances =
            List.map (distanceAfter 2503) (parse input)
    in
    List.maximum distances |> Maybe.withDefault 0


part2 : List String -> Int
part2 input =
    let
        points =
            pointsAfter 2503 (parse input)
    in
    List.maximum points |> Maybe.withDefault 0


distanceAfter : Int -> Reindeer -> Int
distanceAfter time reindeer =
    let
        cycle =
            reindeer.duration + reindeer.rest
    in
    reindeer.speed * (reindeer.duration * (time // cycle) + min reindeer.duration (modBy cycle time))


pointsAfter : Int -> List Reindeer -> List Int
pointsAfter maxTime reindeer =
    let
        pa time tMax points deer =
            let
                distances =
                    List.map (distanceAfter time) deer

                newPoints =
                    List.map2 (+) points (pointsAt time distances deer)
            in
            if time == tMax then
                newPoints

            else
                pa (time + 1) tMax newPoints deer
    in
    pa 1 maxTime (List.repeat (List.length reindeer) 0) reindeer


pointsAt : Int -> List Int -> List Reindeer -> List Int
pointsAt time distances deer =
    let
        maxDist =
            List.maximum distances |> Maybe.withDefault 0

        points d =
            if d == maxDist then
                1

            else
                0
    in
    List.map points distances


parse : List String -> List Reindeer
parse input =
    List.foldl parseLine [] input


parseLine : String -> List Reindeer -> List Reindeer
parseLine text reindeer =
    let
        regex =
            "(\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+)"
    in
    case submatches regex text of
        [ Just speed, Just duration, Just rest ] ->
            reindeer ++ [ Reindeer (toInt speed) (toInt duration) (toInt rest) ]

        _ ->
            reindeer
