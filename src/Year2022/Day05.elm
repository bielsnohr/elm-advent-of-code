module Year2022.Day05 exposing (..)

import Performance
import Result.Extra as Result
import Parser exposing ((|.), (|=), Parser)
import Util.Parser
import Parser exposing (succeed)
import Parser exposing (symbol)
import Parser exposing (loop)
import Parser exposing (oneOf)
import Parser exposing (Step(..))
import Parser exposing (chompIf)
import Parser exposing (getChompedString)
import List.Extra exposing (transpose, getAt, splitAt, last)
import Parser exposing (spaces)
import Parser exposing (int)
import Parser exposing (keyword)
import List exposing (reverse)
import List exposing (append)
import List exposing (head)
import List.Extra exposing (setAt)


solution =
    { solve = solve
    , title = "Supply Stacks"
    , subtitle = "Figure out which crates end up on which stacks. Note: you need to get rid of the index line in the input."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        split_input = input |> String.split "\n\n"

        parsed_stacks = 
          List.head split_input 
            |> Maybe.withDefault ""
            |> String.split "\n"
            |> List.map (Parser.run stackParser)

        stack_init =
            if List.any Result.isErr parsed_stacks then
              [[]]
            else
              parsed_stacks 
                |> List.map (Result.withDefault [])
                |> transpose 
                |> removeEmptyStrings

        instructions =
          List.head (reverse split_input)
            |> Maybe.withDefault ""
            |> Parser.run (Util.Parser.parseRowsUsing instructionsParser)
            |> Result.mapError Util.Parser.firstErrorMsg

        r1 =
            case (stack_init, instructions) of
                (stacks, Ok instrucs) ->
                    List.foldl applyInstruction stacks instrucs
                        |> getTopCrates |> Ok
                (_, _) ->
                    Err "failure"

        r2 =
            case (stack_init, instructions) of
                (stacks, Ok instrucs) ->
                    List.foldl apply9001Instruction stacks instrucs
                        |> getTopCrates |> Ok
                (_, _) ->
                    Err "failure"

    in
    ( r1
    , r2
    )

stackParser : Parser (List String)
stackParser =
    loop []
        (\crates ->
            oneOf
                [ succeed (\char -> Loop (char :: crates))
                    |. symbol "["
                    |= getChompedString (chompIf Char.isAlpha)
                    |. symbol "]"
                , succeed (\_ -> Loop ("" :: crates))
                    |= symbol (String.repeat 4 " ")
                , succeed (\_ -> Loop (crates))
                    |= chompIf (\c -> c == ' ')
                , succeed (Done <| List.reverse crates)
                ]
        )

type alias MoveInstruction =
    { numCrates : Int
    , fromIndex : Int
    , toIndex : Int
    }

createMoveInstruction : Int -> Int -> Int -> MoveInstruction
createMoveInstruction num from to =
  MoveInstruction num (from - 1) (to - 1)

type alias Stacks =
  List (List String)


instructionsParser : Parser MoveInstruction
instructionsParser =
  succeed createMoveInstruction
    |. keyword "move"
    |. spaces
    |= int
    |. spaces
    |. keyword "from"
    |. spaces
    |= int
    |. spaces
    |. keyword "to"
    |. spaces
    |= int

removeEmptyStrings : List (List String) -> List (List String)
removeEmptyStrings ll =
  let
      filter_empty = List.filter (\c -> not <| c == "")
  in
  ll |> List.map filter_empty

applyInstruction : MoveInstruction -> Stacks -> Stacks
applyInstruction move stacks =
  let
      from_stack_split = splitAt move.numCrates <| Maybe.withDefault [] <| getAt move.fromIndex stacks
      new_from_stack = Tuple.second from_stack_split
      moved_crates = Tuple.first from_stack_split |> reverse
      old_to_stack = Maybe.withDefault [] <| getAt move.toIndex stacks
      new_to_stack = append moved_crates old_to_stack
  in
  stacks
    |> setAt move.fromIndex new_from_stack
    |> setAt move.toIndex new_to_stack

apply9001Instruction : MoveInstruction -> Stacks -> Stacks
apply9001Instruction move stacks =
  let
      from_stack_split = splitAt move.numCrates <| Maybe.withDefault [] <| getAt move.fromIndex stacks
      new_from_stack = Tuple.second from_stack_split
      moved_crates = Tuple.first from_stack_split
      old_to_stack = Maybe.withDefault [] <| getAt move.toIndex stacks
      new_to_stack = append moved_crates old_to_stack
  in
  stacks
    |> setAt move.fromIndex new_from_stack
    |> setAt move.toIndex new_to_stack
  
getTopCrates : Stacks -> String
getTopCrates stacks =
  stacks
    |> List.map (head >> Maybe.withDefault "")
    |> String.concat
  
