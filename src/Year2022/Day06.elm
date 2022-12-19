module Year2022.Day06 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Performance
import Result.Extra as Result
import Set
import Util.Parser


solution =
    { solve = solve
    , title = "Tuning Trouble"
    , subtitle = "Fix the handheld communication device."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        datastream =
            Parser.run Util.Parser.alpha input
                |> Result.map String.toList

        r1 =
            case datastream of
                Ok data ->
                    case findMarker 1 data of
                        Just index ->
                            Ok (String.fromInt index)

                        Nothing ->
                            Err "no marker found"

                Err _ ->
                    Err "parsing failure"

        r2 =
            case datastream of
                Ok data ->
                    case findMessage 1 data of
                        Just index ->
                            Ok (String.fromInt index)

                        Nothing ->
                            Err "no marker found"

                Err _ ->
                    Err "parsing failure"
    in
    ( r1
    , r2
    )


findPacket : Int -> Int -> List Char -> Maybe Int
findPacket pack_size index data =
    if List.length data < pack_size then
        Nothing

    else
        let
            group =
                Set.fromList <| List.take pack_size data
        in
        if Set.size group == pack_size then
            Just (index + pack_size - 1)

        else
            findPacket pack_size (index + 1) (Maybe.withDefault [] (List.tail data))


findMarker : Int -> List Char -> Maybe Int
findMarker index data =
    findPacket 4 index data


findMessage : Int -> List Char -> Maybe Int
findMessage index data =
    findPacket 14 index data
