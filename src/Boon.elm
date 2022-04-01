module Boon exposing
    ( Expression, parse
    , evalFromDict, evalFromSet, eval, identifiers
    )

{-| Parsing and evaulation of [Boon](https://docs.google.com/document/u/0/d/1UzsnnKjjW7T_u-OPb5dcPmc9My4YS_jHoyButolNVa4/mobilebasic#heading=h.wazvxi10wtkz) strings.


# Terminology

In the boon expression:

`this OR that`

`OR` is an _operator_, and `this` and `that` are _identifiers_.


# Parse

@docs Expression, parse


# Evaluate

@docs evalFromDict, evalFromSet, eval, identifiers

-}

import Dict exposing (Dict)
import Internals exposing (Expr)
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

    -- Evaluates to True
    parse "foo OR bar"
        |> evalFromDict values

    -- Evaluates to False
    parse "baz OR bar"
        |> evalFromDict values

-}
evalFromDict : Dict String Bool -> Expression -> Bool
evalFromDict values expr =
    eval (\identifier -> Dict.get identifier values |> Maybe.withDefault False)
        expr


{-| Evalute an expression by looking up identifiers in a `Set String`. Identifiers
present in the set are `True`, any others are `False`.

    values = Set.fromList ["foo", "bar"]

    -- Evaluates to True
    parse "foo AND bar"
        |> evalFromSet values

    -- Evaluates to False
    parse "bar AND baz"
        |> evalFromSet values

-}
evalFromSet : Set String -> Expression -> Bool
evalFromSet values expr =
    eval (\identifier -> Set.member identifier values)
        expr


{-| Evaluate an expression based on a user-provided lookup function that maps
identifiers to boolean values.

    trueOrFalse str =
        if (str == "true") then
            True
        else
            False

    -- Evaluates to False
    parse "true AND false"
        |> eval trueOrFalse

-}
eval : (String -> Bool) -> Expression -> Bool
eval lookupFn (Expr expr) =
    Internals.evaluate lookupFn expr


{-| Get the set of identifiers used in an expression. This can be useful if
you need to fetch these identifiers from some external source with a Cmd prior
to evaluation, or validate that they meet some specific format.
-}
identifiers : Expression -> Set String
identifiers (Expr expr) =
    Internals.identifiers Set.empty expr
