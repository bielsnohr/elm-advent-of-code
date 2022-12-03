module Year2022.Day01 exposing (..)

import Performance exposing (Performance)
import Result.Extra as Result
import Parser exposing ((|.), (|=), Parser)
import Util.Parser


solution =
    { solve = solve
    , title = "Calorie Counting"
    , subtitle = "Calculate some metrics to quantify the calories the elves are carrying."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        numbers =
            input
                |> String.split "\n\n"
                |> List.map (Parser.run <| Util.Parser.parseRowsUsing Parser.int)
                |> List.map (Result.mapError Util.Parser.firstErrorMsg)

        r1 =
            numbers
            |> sumCaloriesCarried
            |> List.maximum
            |> Result.fromMaybe "No maximum found"
            |> Result.map String.fromInt

        r2 =
            numbers
            |> sumCaloriesCarried
            |> List.sort
            |> List.reverse
            |> List.take 3
            |> List.sum
            |> String.fromInt
            -- I'm not sure how to handle the number to Result conversion here...
            |> Ok
    in
    ( r1
    , r2
    )

sumCaloriesCarried : List (Result String (List Int)) -> List Int
sumCaloriesCarried snacksPerElf =
    snacksPerElf |> List.map (Result.withDefault [0] >> List.sum)
