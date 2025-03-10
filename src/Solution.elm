module Solution exposing (..)

import Performance exposing (Performance)
import Year2022.Day01
import Year2022.Day02
import Year2022.Day03
import Year2022.Day04
import Year2022.Day05
import Year2022.Day06
import Year2022.Day07


type alias Solution =
    { solve : String -> ( Result String String, Result String String )
    , title : String
    , subtitle : String
    , tests : List ( String, String )
    , performance : Performance
    }


for : Int -> Int -> Maybe Solution
for year day =
    -- TODO replace this with file system searching
    case year of
        2022 ->
            case day of
                1 ->
                    Just Year2022.Day01.solution

                2 ->
                    Just Year2022.Day02.solution

                3 ->
                    Just Year2022.Day03.solution

                4 ->
                    Just Year2022.Day04.solution

                5 ->
                    Just Year2022.Day05.solution

                6 ->
                    Just Year2022.Day06.solution

                7 ->
                    Just Year2022.Day07.solution

                _ ->
                    Nothing

        _ ->
            Nothing
