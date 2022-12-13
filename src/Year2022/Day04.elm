module Year2022.Day04 exposing (..)

import Performance exposing (Performance)
import Result.Extra as Result
import Parser exposing ((|.), (|=), Parser)
import Util.Parser
import Parser exposing (succeed)
import Parser exposing (symbol)


solution =
    { solve = solve
    , title = "Camp Cleanup"
    , subtitle = "Reconcile the cleaning sections that elves were allocated."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        cleaning_sections =
            input
                |> Parser.run (Util.Parser.parseRowsUsing cleaningSectionsParser)
                |> Result.mapError Util.Parser.firstErrorMsg

        r1 =
            case cleaning_sections of
                Ok sections ->
                    Ok (List.map quantifySectionsCompleteOverlap sections |> List.sum |> String.fromInt)
                Err err ->
                    Err err

        r2 =
            case cleaning_sections of
                Ok sections ->
                    Ok (List.map quantifySectionsAnyOverlap sections |> List.sum |> String.fromInt)
                Err err ->
                    Err err
    in
    ( r1
    , r2
    )

type alias CleaningSections =
    { elf1_l : Int
    , elf1_u : Int
    , elf2_l : Int
    , elf2_u : Int
    }

cleaningSectionsParser : Parser CleaningSections
cleaningSectionsParser =
    succeed CleaningSections
        |= Parser.int
        |. symbol "-"
        |= Parser.int
        |. symbol ","
        |= Parser.int
        |. symbol "-"
        |= Parser.int

quantifySectionsCompleteOverlap : CleaningSections -> Int
quantifySectionsCompleteOverlap sections =
    let
        elf1Overlapped = (sections.elf1_l <= sections.elf2_l) && (sections.elf1_u >= sections.elf2_u)
        elf2Overlapped = (sections.elf2_l <= sections.elf1_l) && (sections.elf2_u >= sections.elf1_u)
    in
        if elf1Overlapped || elf2Overlapped then
            1
        else
            0
    
quantifySectionsAnyOverlap : CleaningSections -> Int
quantifySectionsAnyOverlap sections =
    let
        partial1 = (sections.elf1_l <= sections.elf2_u) && (sections.elf1_l >= sections.elf2_l)
        partial2 = (sections.elf1_u <= sections.elf2_u) && (sections.elf1_u >= sections.elf2_l)
        
    in
        if partial1 || partial2 then
            1
        else if (quantifySectionsCompleteOverlap sections) == 1 then
            1
        else
            0
