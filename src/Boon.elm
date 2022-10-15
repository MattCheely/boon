module Boon exposing
    ( Expression, parse, parseWith
    , eval, evalFromDict, evalFromSet, identifiers
    )

{-| Parsing and evaulation of [Boon](https://docs.google.com/document/u/0/d/1UzsnnKjjW7T_u-OPb5dcPmc9My4YS_jHoyButolNVa4/mobilebasic#heading=h.wazvxi10wtkz) strings.


# Terminology

In the boon expression:

`this OR that`

`OR` is an _operator_, and `this` and `that` are _identifiers_.


# Parsing

@docs Expression, parse, parseWith


# Evaluation

@docs eval, evalFromDict, evalFromSet, identifiers

-}

import Dict exposing (Dict)
import Internals exposing (Expr)
import Set exposing (Set)


{-| A parsed Boon expression with identifiers of type `i`. Can be evaluated many times
-}
type Expression i
    = Expr (Internals.Expr i)


{-| Try to parse a string as a Boon expression, failing on invalid input.
-}
parse : String -> Result String (Expression String)
parse input =
    Internals.parse Ok input
        |> Result.map Expr


{-| Try to parse a string as a Boon expression, using the provided function to transform
identifiers. This can be useful if your Boon expressions have more than one type of identifier
in them and you want to map them to custom types.

    type Ingredient
        = Meat String
        | Cheese String

    parseIngredient : String -> Result String Ingredient
    parseIngredient str =
        if String.startsWith "meat:" str then
            Ok (Meat (String.dropLeft 5 str))

        else if String.startsWith "cheese:" str then
            Ok (Cheese (String.dropLeft 7 str))

        else
            Err (str ++ " is not a valid ingredient")

    sandwichCheck : Ingredient -> Bool
    sandwichCheck ingredient =
        case ingredient of
            Meat "ham" ->
                True
            Cheese "swiss" ->
                True

    -- returns True
    parseWith parseIngredient "meat:ham AND cheese:swiss"
        |> eval sandwichCheck

-}
parseWith : (String -> Result String i) -> String -> Result String (Expression i)
parseWith identifierParser input =
    Internals.parse identifierParser input
        |> Result.map Expr


{-| Evaluate an expression by looking up identifiers in a `Dict`. If a
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
evalFromDict : Dict comparable Bool -> Expression comparable -> Bool
evalFromDict values expr =
    eval (\identifier -> Dict.get identifier values |> Maybe.withDefault False)
        expr


{-| Evalute an expression by looking up identifiers in a `Set`. Identifiers
present in the set are `True`, any others are `False`.

    values = Set.fromList ["foo", "bar"]

    -- Evaluates to True
    parse "foo AND bar"
        |> evalFromSet values

    -- Evaluates to False
    parse "bar AND baz"
        |> evalFromSet values

-}
evalFromSet : Set comparable -> Expression comparable -> Bool
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
eval : (i -> Bool) -> Expression i -> Bool
eval lookupFn (Expr expr) =
    Internals.evaluate lookupFn expr


{-| Get a list of identifiers used in an expression. This can be useful if
you need to fetch these identifiers from some external source with a Cmd prior
to evaluation, or otherwise keep a list of them for reference.

Identifiers may be included more than once and ordering is not guaranteed. It is
recommended to collect them in a `Set` if the identifiers are `comparable`.

-}
identifiers : Expression i -> List i
identifiers (Expr expr) =
    Internals.identifiers [] expr
