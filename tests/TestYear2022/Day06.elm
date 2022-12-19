module TestYear2022.Day06 exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra exposing (transpose)
import Parser exposing (Parser)
import Test exposing (..)
import Util.Parser
import Year2022.Day06 exposing (..)


test_findMarker : Test
test_findMarker =
    test "Verify the marker can be found at the correct index in the datastream." <|
        \_ ->
            let
                datastream =
                    "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
            in
            datastream
                |> String.toList
                |> findMarker 1
                |> Expect.equal (Just 7)


test_findMessage : Test
test_findMessage =
    test "Verify the message can be found at the correct index in the datastream." <|
        \_ ->
            let
                datastream =
                    "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
            in
            datastream
                |> String.toList
                |> findMessage 1
                |> Expect.equal (Just 19)
