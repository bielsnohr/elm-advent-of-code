module TestYear2022.Day05 exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra exposing (transpose)
import Parser exposing (Parser)
import Test exposing (..)
import Util.Parser
import Year2022.Day05 exposing (..)


test_stackParser : Test
test_stackParser =
    test "Verify the initial stack state parser is working." <|
        \_ ->
            let
                stack_line =
                    "    [A]    [B] [C]"
            in
            stack_line
                |> Parser.run stackParser
                |> Expect.equal (Ok [ "", "A", "", "B", "C" ])


test_stackParser_with_indices : Test
test_stackParser_with_indices =
    test "Verify the stack parser works with the index line." <|
        \_ ->
            let
                stack_line =
                    " 1  2  3 "
            in
            stack_line
                |> Parser.run stackParser
                |> Expect.equal (Ok [])


test_transpose : Test
test_transpose =
    test "Verify the transpose operation used on the input stack." <|
        \_ ->
            let
                input_stacks =
                    [ [ "", "A", "" ], [ "B", "C", "D" ] ]
            in
            input_stacks
                |> transpose
                |> Expect.equal [ [ "", "B" ], [ "A", "C" ], [ "", "D" ] ]


test_removeEmptyStrings : Test
test_removeEmptyStrings =
    test "Verify that empty string elements are removed from a list of lists." <|
        \_ ->
            let
                input_stacks =
                    [ [ "A", "", "" ], [ "B", "C", "" ] ]
            in
            input_stacks
                |> removeEmptyStrings
                |> Expect.equal [ [ "A" ], [ "B", "C" ] ]


test_instructionParser =
    test "Verify that we can parse the crate moving instructions." <|
        \_ ->
            let
                instruction =
                    "move 1 from 2 to 3"

                move_instruction =
                    MoveInstruction 1 1 2
            in
            instruction
                |> Parser.run instructionsParser
                |> Expect.equal (Ok move_instruction)


test_applyInstruction : Test
test_applyInstruction =
    test "Verify instructions are applied properly to stacks." <|
        \_ ->
            let
                input_stacks =
                    [ [ "D", "N", "Z" ], [ "C", "M" ], [ "P" ] ]

                move_instruction =
                    createMoveInstruction 3 1 3

                output_stacks =
                    [ [], [ "C", "M" ], [ "Z", "N", "D", "P" ] ]
            in
            applyInstruction move_instruction input_stacks
                |> Expect.equal output_stacks


test_getTopCrates : Test
test_getTopCrates =
    test "Verify the top crates of each stack is retrieved." <|
        \_ ->
            let
                input_stacks =
                    [ [ "D", "N", "Z" ], [ "C", "M" ], [ "P" ] ]

                top_crates =
                    "DCP"
            in
            getTopCrates input_stacks
                |> Expect.equal top_crates
