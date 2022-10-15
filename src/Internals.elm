module Internals exposing (Expr(..), evaluate, identifiers, parse)

import Errors
import List.Extra as ListX
import Parser exposing (..)
import Set



-- EXPRESSIONS


type Expr a
    = Identifier a
    | Not (Expr a)
    | Xor (Expr a) (Expr a)
    | And (Expr a) (Expr a)
    | Or (Expr a) (Expr a)


evaluate : (a -> Bool) -> Expr a -> Bool
evaluate lookupFn expr =
    case expr of
        Identifier i ->
            lookupFn i

        Not expr1 ->
            not (evaluate lookupFn expr1)

        Xor expr1 expr2 ->
            xor (evaluate lookupFn expr1) (evaluate lookupFn expr2)

        And expr1 expr2 ->
            evaluate lookupFn expr1 && evaluate lookupFn expr2

        Or expr1 expr2 ->
            evaluate lookupFn expr1 || evaluate lookupFn expr2


parse : (String -> Result String a) -> String -> Result String (Expr a)
parse idParser string =
    string
        |> run
            (succeed identity
                |= expression idParser
                |. end
            )
        |> Result.mapError Errors.toString


identifiers : List a -> Expr a -> List a
identifiers acc expr =
    case expr of
        Identifier i ->
            i :: acc

        Not expr1 ->
            identifiers acc expr1

        Xor expr1 expr2 ->
            acc
                |> (\ids -> identifiers ids expr1)
                |> (\ids -> identifiers ids expr2)

        And expr1 expr2 ->
            acc
                |> (\ids -> identifiers ids expr1)
                |> (\ids -> identifiers ids expr2)

        Or expr1 expr2 ->
            acc
                |> (\ids -> identifiers ids expr1)
                |> (\ids -> identifiers ids expr2)



-- PARSER


{-| Identifiers are strings that can be mapped to a boolean value
-}
identifier : (String -> Result String a) -> Parser (Expr a)
identifier idParser =
    oneOf
        [ unQuotedVar idParser
        , quotedVar idParser
        ]


unQuotedVar : (String -> Result String a) -> Parser (Expr a)
unQuotedVar idParser =
    let
        invalidChars =
            Set.union separatorChars structuralChars
                |> Set.insert '#'
    in
    variable
        { start = \c -> not (Set.member c invalidChars) && not (c == '"')
        , inner = \c -> not (Set.member c invalidChars)
        , reserved = Set.fromList [ "NOT", "AND", "OR", "XOR" ]
        }
        |> Parser.andThen
            (\str ->
                case idParser str of
                    Ok id ->
                        succeed (Identifier id)

                    Err err ->
                        problem err
            )


quotedVar : (String -> Result String a) -> Parser (Expr a)
quotedVar idParser =
    succeed identity
        |. token "\""
        |= loop [] qVarHelp
        |> andThen
            (\str ->
                if String.isEmpty str then
                    problem "Quoted identifiers must not be empty"

                else
                    case idParser str of
                        Ok id ->
                            succeed (Identifier id)

                        Err err ->
                            problem err
            )


qVarHelp : List String -> Parser (Step (List String) String)
qVarHelp revChunks =
    oneOf
        -- capture escaped quote
        [ token "\\\""
            |> map (\_ -> Loop ("\"" :: revChunks))

        -- capture an isolated slash
        , token "\\"
            |> map (\_ -> Loop ("\\" :: revChunks))

        -- end on an unescaped quote
        , token "\""
            |> map (\_ -> Done (String.join "" (List.reverse revChunks)))

        -- capture until we see a quote or slash
        , chompWhile (\c -> c /= '"' && c /= '\\')
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: revChunks))
        ]


{-| Parser for separator characters, very similar to `spaces` in the parser library
-}
separators : Parser ()
separators =
    chompWhile (\c -> Set.member c separatorChars)


separatorChars =
    Set.fromList [ ' ', '\t', '\n', '\u{000D}' ]


structuralChars =
    Set.fromList [ '(', ')' ]


{-| This parses all the stuff we should just ignore
-}
spacesAndComments : Parser ()
spacesAndComments =
    loop 0 spaceAndCommentHelp


spaceAndCommentHelp : Int -> Parser (Step Int ())
spaceAndCommentHelp lastOffset =
    oneOf
        [ succeed Loop
            |. lineComment "#"
            |= getOffset
        , succeed
            (\offset ->
                if offset == lastOffset then
                    Done ()

                else
                    Loop offset
            )
            |. separators
            |= getOffset
        ]


{-| Infix operator parsing. There's probably a way to incorporate
NOT into this, but I found it conceptually easier to parse NOTs
in the term parser, since it's the highest precedence operation
-}
type Operator
    = AndOp
    | OrOp
    | XorOp


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> AndOp) (keyword "AND")
        , map (\_ -> OrOp) (keyword "OR")
        , map (\_ -> XorOp) (keyword "XOR")
        ]


{-| A term is a standalone chunk of logic, like `identifier` or `(one AND two)`. We use it as
a building block in larger expressions.
-}
term : (String -> Result String a) -> Parser (Expr a)
term idParser =
    succeed identity
        |. spacesAndComments
        |= oneOf
            [ identifier idParser
            , backtrackable <|
                succeed Not
                    |. keyword "NOT"
                    |. spacesAndComments
                    |= identifier idParser
            , succeed Not
                |. keyword "NOT"
                |. spacesAndComments
                |. symbol "("
                |. spacesAndComments
                |= lazy (\_ -> expression idParser)
                |. spacesAndComments
                |. symbol ")"
            , succeed identity
                |. symbol "("
                |. spacesAndComments
                |= lazy (\_ -> expression idParser)
                |. spacesAndComments
                |. symbol ")"
            ]
        |. spacesAndComments


{-| Every expression starts with a term. After that, it may be done, or there
may be a `AND`, `XOR`, or `OR` sign and more logic.
-}
expression : (String -> Result String a) -> Parser (Expr a)
expression idParser =
    term idParser
        |> andThen (\expr -> loop ( [], expr ) (expressionHelp idParser))


{-| Once you have parsed a term, you can start looking for other operators.
I am tracking everything as a list, that way I can be sure to follow the order
of operations (PEMDAS) when building the final expression.
In one case, I need an operator and another term. If that happens I keep
looking for more. In the other case, I am done parsing, and I finalize the
expression.
-}
expressionHelp : (String -> Result String a) -> ( List ( Expr a, Operator ), Expr a ) -> Parser (Step ( List ( Expr a, Operator ), Expr a ) (Expr a))
expressionHelp idParser ( revOps, expr ) =
    oneOf
        [ succeed Tuple.pair
            |. spacesAndComments
            |= operator
            |. spacesAndComments
            |= term idParser
            |> map
                (\( op, newExpr ) ->
                    Loop ( ( expr, op ) :: revOps, newExpr )
                )
        , lazy
            (\_ ->
                succeed (Done (finalize revOps expr))
            )
        ]


{-| We fold up our flat list of terms and operators in the order of
operator precedence. Then we finish with a generic fold that shouldn't
do anything in practice due to the previous steps, but guarantees
for the compiler that we are left with a single Expression
-}
finalize : List ( Expr a, Operator ) -> Expr a -> Expr a
finalize revOps finalExpr =
    ( revOps, finalExpr )
        |> foldOperator XorOp
        |> foldOperator AndOp
        |> foldOperator OrOp
        |> finish


{-| The second branch of this case normally isn't called, but it
guarantees there is only a single Expr at the end by folding all
operators into our parsed tree with equal precedence. It also
helps illustrate what the more complex foldOp and foldOperator
functions are doing at their core.
-}
finish : ( List ( Expr a, Operator ), Expr a ) -> Expr a
finish ( opList, final ) =
    case opList of
        [] ->
            final

        -- This shouldn't normally do anything
        ( term1, op ) :: rest ->
            finish ( rest, construct op term1 final )


{-| `indexedFoldr` helper for `foldOperator`.
Given a targetOperator, the current index in the list of operators,
the term/operator pair at that index, and an intermediate accumulator,
turn all instances of the target operator into Exprs and fold them in
with the other operators in the list. For example, if our string was:
"first XOR second AND third XOR fourth"
our list of terms before finalizing would look like (in simplified notation):
([(third, XorOp), (second, AndOp), (first XorOp)], fourth)
After folding the Xors, it should look like:
([((Xor first second) AndOp)], (Xor third fourth))
Note that we have to fold one Xor into the AndOp, and one into the final term.
-}
foldOp :
    Operator
    -> Int
    -> ( Expr a, Operator )
    -> ( List ( Expr a, Operator ), Expr a )
    -> ( List ( Expr a, Operator ), Expr a )
foldOp targetOp idx ( nextTerm, nextOp ) ( collected, final ) =
    -- If this is the first item in the list (and the last in sequence order)
    -- and it's the target operation, fold it into the final term
    case collected of
        -- if the previous op we collected was a target operation, fold it into the
        -- next term
        ( prevTerm, prevOp ) :: rest ->
            case ( prevOp == targetOp, idx == 0 && nextOp == targetOp ) of
                -- We need to fold into the previous expr, but not with the final expr
                ( True, False ) ->
                    ( ( construct prevOp prevTerm nextTerm, nextOp ) :: rest, final )

                -- We need to fold into the final expr, but not with the previous expr
                ( False, True ) ->
                    ( collected, construct nextOp nextTerm final )

                -- We need to fold into both
                ( True, True ) ->
                    ( rest, construct nextOp (construct prevOp prevTerm nextTerm) final )

                -- Nothing to do, just add it to the collection for proccessing later
                ( False, False ) ->
                    ( ( nextTerm, nextOp ) :: collected, final )

        _ ->
            -- Nothing to do, just add it to the collection for proccessing later
            ( ( nextTerm, nextOp ) :: collected, final )


{-| Run foldOp across the list for a target operator
-}
foldOperator : Operator -> ( List ( Expr a, Operator ), Expr a ) -> ( List ( Expr a, Operator ), Expr a )
foldOperator targetOp ( opList, final ) =
    ListX.indexedFoldr (foldOp targetOp) ( [], final ) opList


construct : Operator -> (Expr a -> Expr a -> Expr a)
construct op =
    case op of
        XorOp ->
            Xor

        AndOp ->
            And

        OrOp ->
            Or
