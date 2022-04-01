module EvalTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internals exposing (Expr(..), evaluate, identifiers)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Evaluation of expressions"
        [ test "works for AND expressions" <|
            \_ ->
                checkTable And
                    [ ( True, True, True )
                    , ( True, False, False )
                    , ( False, True, False )
                    , ( False, False, False )
                    ]
        , test "works for OR expressions" <|
            \_ ->
                checkTable Or
                    [ ( True, True, True )
                    , ( True, False, True )
                    , ( False, True, True )
                    , ( False, False, False )
                    ]
        , test "works for XOR expressions" <|
            \_ ->
                checkTable Xor
                    [ ( True, True, False )
                    , ( True, False, True )
                    , ( False, True, True )
                    , ( False, False, False )
                    ]
        , test "works for NOT true expressions" <|
            \_ ->
                evaluate lookup (Not (Identifier "true"))
                    |> Expect.equal False
        , test "works for NOT false expressions" <|
            \_ ->
                evaluate lookup (Not (Identifier "false"))
                    |> Expect.equal True
        , test "works for complex expressions" <|
            \_ ->
                evaluate lookup
                    (And
                        (And (Not (Identifier "false"))
                            (Identifier "true")
                        )
                        (Xor
                            (Not (Identifier "false"))
                            (Not (Identifier "true"))
                        )
                    )
                    |> Expect.equal True
        , test "provides access to identifiers in an expression" <|
            \_ ->
                identifiers Set.empty
                    (And
                        (And (Not (Identifier "one"))
                            (Identifier "two")
                        )
                        (Xor
                            (Not (Identifier "three"))
                            (Or (Identifier "four") (Identifier "two"))
                        )
                    )
                    |> Expect.equal (Set.fromList [ "one", "two", "three", "four" ])
        ]


bools : Dict String Bool
bools =
    Dict.fromList [ ( "true", True ), ( "false", False ) ]


lookup : String -> Bool
lookup identifier =
    Dict.get identifier bools
        |> Maybe.withDefault False


checkTable : (Expr -> Expr -> Expr) -> List ( Bool, Bool, Bool ) -> Expectation
checkTable op table =
    logicTable op table
        |> List.map (\( expr, res ) -> Expect.equal (evaluate lookup expr) res)
        |> List.map always
        |> (\expects -> Expect.all expects ())


logicTable : (Expr -> Expr -> Expr) -> List ( Bool, Bool, Bool ) -> List ( Expr, Bool )
logicTable op table =
    table
        |> List.map
            (\( in1, in2, out ) ->
                ( op (boolToken in1) (boolToken in2), out )
            )


boolToken : Bool -> Expr
boolToken b =
    if b then
        Identifier "true"

    else
        Identifier "false"
