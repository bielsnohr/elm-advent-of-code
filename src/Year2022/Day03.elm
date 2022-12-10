-- Use this as a base for daily solutions
module Year2022.Day03 exposing (..)

import Performance exposing (Performance)
import Result.Extra as Result
import Parser exposing ((|.), (|=), Parser)
import Util.Parser
import Parser exposing (succeed)
import Parser exposing (getChompedString)
import Parser exposing (chompIf)
import Parser exposing (spaces)
import Set


solution =
    { solve = solve
    , title = "Rucksack Reorganization"
    , subtitle = "Find the item type errors in the rucksacks."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        rucksack_strings =
            input
                |> Parser.run (Util.Parser.parseRowsUsing Util.Parser.alpha)
                |> Result.mapError Util.Parser.firstErrorMsg

        r1 =
            case rucksack_strings of
                Ok strings ->
                    Ok (List.map calculateRucksackPriority strings |> List.sum |> String.fromInt)
                Err err ->
                    Err err

        r2 =
            Err "--empty--"
    in
    ( r1
    , r2
    )

calculateRucksackPriority : String -> Int
calculateRucksackPriority rucksack =
    let
        wrong_item = 
            findWrongItem rucksack
    in

        case wrong_item of
            Just item ->
                if Char.isUpper item then
                    (Char.toCode item) - 38
                else
                    (Char.toCode item) - 96
            Nothing ->
                0
        
    
    
findWrongItem : String -> Maybe Char
findWrongItem rucksack =
    let
        length = (String.length rucksack) // 2
        item_types_1 = String.left length rucksack
                    |> String.toList
                    |> Set.fromList
        item_types_2 = String.right length rucksack
                    |> String.toList
                    |> Set.fromList
    in
        Set.intersect item_types_1 item_types_2 |> Set.toList |> List.head
