module ParseTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internals exposing (Expr(..), parse)
import Test exposing (..)


basicCases =
    [ ( "one", Identifier "one" )
    , ( "oneANDtwo", Identifier "oneANDtwo" )
    , ( "NOT one", Not (Identifier "one") )
    , ( "one AND two", And (Identifier "one") (Identifier "two") )
    , ( "one OR two", Or (Identifier "one") (Identifier "two") )
    , ( "one XOR two", Xor (Identifier "one") (Identifier "two") )
    ]


separatorCases =
    [ ( "one\nAND two", And (Identifier "one") (Identifier "two") )
    , ( "one\tAND two", And (Identifier "one") (Identifier "two") )
    , ( "one\u{000D}AND two", And (Identifier "one") (Identifier "two") )
    , ( "NOT one   ", Not (Identifier "one") )
    , ( "   NOT two", Not (Identifier "two") )
    ]


precedenceCases =
    [ ( "first AND second AND third"
      , And
            (And (Identifier "first") (Identifier "second"))
            (Identifier "third")
      )
    , ( "first AND second AND third AND fourth"
      , And
            (And
                (And (Identifier "first") (Identifier "second"))
                (Identifier "third")
            )
            (Identifier "fourth")
      )
    , ( "first OR second AND third AND fourth"
      , Or (Identifier "first")
            (And
                (And (Identifier "second") (Identifier "third"))
                (Identifier "fourth")
            )
      )
    , ( "first AND second OR third"
      , Or
            (And (Identifier "first") (Identifier "second"))
            (Identifier "third")
      )
    , ( "first XOR second AND third OR fourth"
      , Or
            (And
                (Xor (Identifier "first") (Identifier "second"))
                (Identifier "third")
            )
            (Identifier "fourth")
      )
    , ( "first OR second AND third"
      , Or (Identifier "first")
            (And (Identifier "second") (Identifier "third"))
      )
    , ( "first AND second XOR third OR fourth"
      , Or
            (And (Identifier "first")
                (Xor (Identifier "second")
                    (Identifier "third")
                )
            )
            (Identifier "fourth")
      )
    , ( "first OR second AND third XOR fourth"
      , Or (Identifier "first")
            (And (Identifier "second")
                (Xor (Identifier "third")
                    (Identifier "fourth")
                )
            )
      )
    , ( "first OR second XOR third AND fourth"
      , Or (Identifier "first")
            (And
                (Xor (Identifier "second")
                    (Identifier "third")
                )
                (Identifier "fourth")
            )
      )
    , ( "first OR NOT second XOR third AND NOT fourth"
      , Or (Identifier "first")
            (And
                (Xor (Not (Identifier "second"))
                    (Identifier "third")
                )
                (Not (Identifier "fourth"))
            )
      )
    ]


parenCases =
    [ ( "(first OR second) AND third"
      , And (Or (Identifier "first") (Identifier "second"))
            (Identifier "third")
      )
    , ( "first XOR (second AND third)"
      , Xor (Identifier "first")
            (And (Identifier "second") (Identifier "third"))
      )
    , ( "first XOR (second AND third) OR fourth"
      , Or
            (Xor (Identifier "first")
                (And (Identifier "second") (Identifier "third"))
            )
            (Identifier "fourth")
      )
    , ( "NOT ((first OR second) AND NOT third) XOR (NOT fourth AND fifth)"
      , Xor
            (Not
                (And (Or (Identifier "first") (Identifier "second"))
                    (Not (Identifier "third"))
                )
            )
            (And (Not (Identifier "fourth"))
                (Identifier "fifth")
            )
      )
    ]


commentCases =
    [ ( "NOT first # Comment", Not (Identifier "first") )
    , ( """#Comment
        NOT first""", Not (Identifier "first") )
    , ( """# Comment 1
        # Comment 2
        NOT first
        # Comment 3
        # Comment 4""", Not (Identifier "first") )
    , ( """# Comment
       NOT # Comment
       ( # Comment
       first # Comment
       AND # Comment
       second # Comment
       ) # Comment""", Not (And (Identifier "first") (Identifier "second")) )
    ]


complexCases =
    [ ( "NOT first AND second AND NOT third XOR NOT fourth"
      , And
            (And (Not (Identifier "first"))
                (Identifier "second")
            )
            (Xor (Not (Identifier "third")) (Not (Identifier "fourth")))
      )
    , ( "NOT (first AND second AND NOT third) XOR (NOT fourth XOR fifth)"
      , Xor
            (Not
                (And (And (Identifier "first") (Identifier "second"))
                    (Not (Identifier "third"))
                )
            )
            (Xor (Not (Identifier "fourth"))
                (Identifier "fifth")
            )
      )
    ]


quotedIdentifiers =
    [ ( "\"first\"", Identifier "first" )
    , ( "\"Mr Boole \\\"George\\\"\"", Identifier "Mr Boole \"George\"" )
    , ( "\"has\\a\\slash\"", Identifier "has\\a\\slash" )
    , ( "\"NOT\"", Identifier "NOT" )
    , ( "\"XOR\"", Identifier "XOR" )
    , ( "\"AND\"", Identifier "AND" )
    , ( "\"OR\"", Identifier "OR" )
    , ( "\"\n\"", Identifier "\n" )
    , ( "\"\t\"", Identifier "\t" )
    , ( "\"(\"", Identifier "(" )
    , ( "\")\"", Identifier ")" )
    ]


failureCases =
    [ ( "out of ourder terms", "AND first second" )
    , ( "missing second term", "first AND" )
    , ( "repeated operator", "first AND AND second" )
    , ( "OR with a hanging AND", "first OR second AND" )
    , ( "two NOT operators in a row", "NOT NOT" )
    , ( "isolated NOT operator", "NOT" )
    , ( "isolated nested NOT operator", "(NOT)" )
    , ( "too many closing parentheses", "NOT ((first OR second) AND NOT third))" )
    , ( "too many opening parentheses", "NOT (((first OR second) AND NOT third)" )
    , ( "open parenthesis after identifier", "first (" )
    , ( "empty string", "" )
    , ( "only a comment", "# Comment" )
    , ( "repeated identifiers", "first second" )
    , ( "extra invalid content", "(NOT one)arglebarg" )
    , ( "bad operator capitalization", "first And second" )
    , ( "empty quoted identifier", "\"\"" )
    ]


suite : Test
suite =
    describe "The parser"
        [ describe "parses basic expressions like"
            (List.map testSuccess basicCases)
        , describe "deals correctly with separators / spaces"
            (List.map testSuccess separatorCases)
        , describe "correctly applies operator precedence in"
            (List.map testSuccess precedenceCases)
        , describe "correctly incoporates parentheses"
            (List.map testSuccess parenCases)
        , describe "supports comments"
            (List.map testSuccess commentCases)
        , describe "handles complex cases"
            (List.map testSuccess complexCases)
        , describe "supports quoted identifiers"
            (List.map testSuccess quotedIdentifiers)
        , describe "fails on bad input like"
            (List.map testFailure failureCases)
        ]


type Ingredient
    = Meat String
    | Cheese String
    | Bread String


identifierFromString : String -> Result String Ingredient
identifierFromString str =
    if String.startsWith "meat:" str then
        Ok (Meat (String.dropLeft 5 str))

    else if String.startsWith "cheese:" str then
        Ok (Cheese (String.dropLeft 7 str))

    else if String.startsWith "bread:" str then
        Ok (Bread (String.dropLeft 6 str))

    else
        Err (str ++ " is not a valid ingredient")


testIdentifierParsing =
    describe "Custom Identifier Parsing"
        [ test "Works correctly with valid identifiers" <|
            \_ ->
                parse identifierFromString "meat:ham AND cheese:swiss AND bread:rye"
                    |> Expect.equal (Ok (And (And (Identifier (Meat "ham")) (Identifier (Cheese "swiss"))) (Identifier (Bread "rye"))))
        , test "Fails if any identifier is bad" <|
            \_ ->
                parse identifierFromString "meat:bacon AND veg:tomatoes"
                    |> Expect.err
        ]


testFailure ( description, input ) =
    test description <|
        \_ ->
            parse Ok input
                |> Expect.err


testSuccess ( textExpr, parsed ) =
    test textExpr <|
        \_ ->
            parse Ok textExpr
                |> Expect.equal (Ok parsed)
