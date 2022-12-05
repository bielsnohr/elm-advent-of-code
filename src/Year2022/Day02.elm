-- Use this as a base for daily solutions
module Year2022.Day02 exposing (..)

import Performance exposing (Performance)
import Result.Extra as Result
import Parser exposing ((|.), (|=), Parser)
import Util.Parser
import Parser exposing (succeed)
import Parser exposing (getChompedString)
import Parser exposing (chompIf)
import Parser exposing (spaces)


solution =
    { solve = solve
    , title = "Rock Paper Scissors"
    , subtitle = "Calculate your score in the tournament."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        rounds =
            input
                |> Parser.run (Util.Parser.parseRowsUsing roundParser)
                |> Result.mapError Util.Parser.firstErrorMsg

        r1 =
            case rounds of
                Ok values ->
                    Ok (String.fromInt (List.map scoreRound values |> List.sum))
                Err err ->
                    Err err

        r2 =
            case rounds of
                Ok values ->
                    Ok (String.fromInt (List.map scoreRoundActual values |> List.sum))
                Err err ->
                    Err err
    in
    ( r1
    , r2
    )

roundParser : Parser RoundRPS
roundParser =
    succeed RoundRPS
        |= (getChompedString <| chompIf (\c -> List.member c ['A', 'B', 'C']))
        |. spaces
        |= (getChompedString <| chompIf (\c -> List.member c ['X', 'Y', 'Z']))
    
type alias RoundRPS =
    { opponent : String
    , mine : String
    }

type Outcome
    = Win
    | Lose
    | Draw
    | None

scoreRound : RoundRPS -> Int
scoreRound round =
    let
        -- TODO should deal with potential parsing errors earlier so that I
        -- don't have to make this fudge. What would be the best way to do that?
        mine = Maybe.withDefault 0 (decryptMine round.mine)

        opp = Maybe.withDefault 0 (decryptOpponent round.opponent)
    in
    if (modBy 3 (mine + 1)) == opp then
        mine + 1
    else if mine == opp then
        mine + 4
    else
        mine + 7

scoreRoundActual : RoundRPS -> Int
scoreRoundActual round =
    let
        -- TODO should deal with potential parsing errors earlier so that I
        -- don't have to make this fudge. What would be the best way to do that?
        mine = decryptMineActual round.mine

        opp = Maybe.withDefault 0 (decryptOpponent round.opponent)
    in

    case mine of
        Win ->
            (modBy 3 (opp + 1)) + 7
        Draw ->
            opp + 4
        Lose ->
            (modBy 3 (opp - 1)) + 1
        None ->
            0

            

decryptOpponent : String -> Maybe Int
decryptOpponent opp =
    case opp of
        "A" ->
            Just 0
        "B" ->
            Just 1
        "C" ->
            Just 2
        _ ->
            Nothing

decryptMine : String -> Maybe Int
decryptMine mine =
    case mine of
        "X" ->
            Just 0
        "Y" ->
            Just 1
        "Z" ->
            Just 2
        _ ->
            Nothing

decryptMineActual : String -> Outcome
decryptMineActual mine =
    case mine of
        "X" ->
            Lose
        "Y" ->
            Draw
        "Z" ->
            Win
        _ ->
            None
