module Boon exposing
    ( Expression, parse
    , evalFromDict, evalFromSet, eval
    )

{-| Parsing and evaulation of [Boon](https://docs.google.com/document/u/0/d/1UzsnnKjjW7T_u-OPb5dcPmc9My4YS_jHoyButolNVa4/mobilebasic#heading=h.wazvxi10wtkz) strings.


# Terminology

In the boon expression:

`this OR that`

`OR` is an _operator_, and `this` and `that` are _identifiers_.


# Parse

@docs Expression, parse


# Evaluate

@docs evalFromDict, evalFromSet, eval

-}

import Dict exposing (Dict)
import Internals exposing (Expr, evaluate, parse)
import Set exposing (Set)


{-| A parsed Boon expression. Can be evaluated many times
-}
type Expression
    = Expr Internals.Expr


{-| Try to parse a string as a Boon expression, failing on invalid input.
-}
parse : String -> Result String Expression
parse input =
    Internals.parse input
        |> Result.map Expr


{-| Evaluate an expression by looking up identifiers in a `Dict String Bool`. If a
key is absent, it will evaluate to `False`

    values =
        Dict.fromList
            [ ( "foo", True )
            , ( "bar", False )
            ]

    (parse "foo OR bar"
        |> evalFromDict values) == True

    (parse "baz OR bar"
        |> evalFromDict values) == False

-}
evalFromDict : Dict String Bool -> Expression -> Bool
evalFromDict values expr =
    eval (\identifier -> Dict.get identifier values |> Maybe.withDefault False)
        expr


{-| Evalute an expression by looking up identifiers in a `Set String`. Identifiers
present in the set are `True`, any others are `False`.

    values = Set.fromList ["foo", "bar"]

    (parse "foo AND bar"
        |> evalFromSet values) == True

    (parse "bar AND baz"
        |> evalFromSet values) == False

-}
evalFromSet : Set String -> Expression -> Bool
evalFromSet values expr =
    eval (\identifier -> Set.member identifier values)
        expr


{-| Evaluate an expression based on a user-provided lookup function that maps
identifiers to boolean values.
-}
eval : (String -> Bool) -> Expression -> Bool
eval lookupFn (Expr expr) =
    Internals.evaluate lookupFn expr
