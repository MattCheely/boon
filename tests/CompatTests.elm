module CompatTests exposing (..)

import Compat exposing (cases, values)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Internals exposing (Expr(..), evaluate, parse)
import Test exposing (..)


suite : Test
suite =
    describe "elm/boon is compatible with boon-js"
        (List.map testCompat cases)


testCompat ( expr, result ) =
    test expr <|
        \_ ->
            parse Ok expr
                |> Result.map (evaluate lookup)
                |> Expect.equal (Ok result)


lookup : String -> Bool
lookup identifier =
    Dict.get identifier values
        |> Maybe.withDefault False
