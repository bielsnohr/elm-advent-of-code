module TestYear2022.Day04 exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Year2022.Day04 exposing (..)


test_quantifySectionsCompleteOverlap_valid_complete_overlap : Test
test_quantifySectionsCompleteOverlap_valid_complete_overlap =
    test "Check that an actual complete overlap is recognised" <|
        \_ ->
            let
                complete_overlap = CleaningSections 2 8 3 7
            in
            complete_overlap
                |> quantifySectionsCompleteOverlap
                |> Expect.equal 1
