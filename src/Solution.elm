module Solution exposing (..)

import Performance exposing (Performance)
import Year2022.Day01

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

                _ ->
                    Nothing

        _ ->
            Nothing
