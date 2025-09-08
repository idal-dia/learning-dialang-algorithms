module Parser.SimpleExpr exposing (..)

{-| This modules implements a simplified version of Dialang's expr parser.
-}

import Parser as P exposing ((|.), (|=), Parser)


type Expr
    = E_Val Ref LName
    | E_Int Int
    | E_Float Float
    | E_Placeholder -- $
    | E_Binop Expr Binop Expr -- a + b
    | E_App Expr E_Args -- f(a, b)
    | E_Tag IsGhost LName E_Construct -- #tag(a, b) (#_tag if IsGhost)
    | E_Error LName -- ?err
      -- | E_Lam (List ( Pattern, List E_Term, Expr )) -- { d1 -> e1 | d2 | d3 -> e2 }
    | E_Struct E_Construct -- (a, b, ..c)


type alias E_Args =
    List (E_StructElem ())


type alias E_Construct =
    List (E_StructElem Never)


type E_StructElem arg
    = E_Expr Expr
    | E_Field LName (Maybe Expr) -- .field=val
    | E_Var arg LName VarKind
    | E_KeyVal Expr Expr -- .[12]="hello"
    | E_Extension (Maybe Expr) -- ..struct


type VarKind
    = Simple -- `~val`
    | Field (Maybe LName) -- `~.field` (or `.field=~val` if `Just`)


type E_Term
    = E_LocalDecl LName Expr -- a = 13
    | E_Rebind LName Expr -- ~a = 15
    | E_Do Expr -- print("hello")


type alias LName =
    String


type alias UName =
    String


type alias IsGhost =
    Bool


type Ref
    = Ghost
    | Local
    | Import (NEList UName)


type alias Binop =
    String


type alias NEList a =
    ( a, List a )


expr : Parser Expr
expr =
    expr_ False


exprBlock : Parser Expr
exprBlock =
    expr_ True


expr_ : Bool -> Parser Expr
expr_ isBlock =
    [ val
    , num
    , placeholder
    , error
    , P.lazy (\_ -> tag)
    , P.lazy (\_ -> struct)
    ]
        |> P.oneOf
        |> P.andThen
            (\previous ->
                [ P.lazy (\_ -> app previous)
                , P.succeed previous
                ]
                    |> P.oneOf
            )
        |> P.andThen
            (\previous ->
                [ P.lazy (\_ -> binop previous)
                    |> P.backtrackable
                , P.succeed previous
                ]
                    |> P.oneOf
            )


val : Parser Expr
val =
    P.oneOf
        [ P.succeed (E_Val Ghost)
            |. P.symbol "_"
            |= lname
        , (\ref_acc ->
            P.oneOf
                [ P.succeed (\uname_ -> uname_ :: ref_acc |> P.Loop)
                    |= uname
                    |. P.symbol "."
                , P.map
                    (E_Val
                        (case List.reverse ref_acc of
                            [] ->
                                Local

                            h :: q ->
                                Import ( h, q )
                        )
                        >> P.Done
                    )
                    lname
                ]
          )
            |> P.loop []
        ]


uname : Parser UName
uname =
    name Char.isUpper


lname : Parser LName
lname =
    name Char.isLower


name : (Char -> Bool) -> Parser String
name isValidFirstChar =
    (P.succeed identity
        |. P.chompIf isValidFirstChar
        |. P.chompWhile (\c -> Char.isAlphaNum c || c == '_')
    )
        |> P.getChompedString


num : Parser Expr
num =
    let
        sign isNegative =
            if isNegative then
                negate

            else
                identity

        intOrFloat float =
            let
                trunc =
                    truncate float
            in
            if toFloat trunc == float then
                E_Int trunc

            else
                E_Float float

        number isNegative =
            [ P.symbol "."
                -- this case is to avoid recognizing ".12" as a float
                |. P.problem "floating point numbers must start with a digit, like 0.25"
                |> P.map (always (E_Int 0))
            , P.map (sign isNegative >> intOrFloat) P.float
            ]
                |> P.oneOf
                |> P.backtrackable
    in
    P.oneOf
        [ P.succeed identity
            |. P.symbol "-"
            |= number True
        , number False
        ]


placeholder : Parser Expr
placeholder =
    P.symbol "$"
        |> P.map (always E_Placeholder)


{-| here binops are always right associative
-}
binop : Expr -> Parser Expr
binop previous =
    let
        isValidBinopChar c =
            let
                reserved =
                    ".,()[]{}\"'`#?$@_" |> String.toList
            in
            not (isSpace c || Char.isAlphaNum c || List.member c reserved)
    in
    P.succeed (E_Binop previous)
        |. P.spaces
        |= ((P.succeed identity
                |. P.chompIf isValidBinopChar
                |. P.chompWhile isValidBinopChar
            )
                |> P.getChompedString
           )
        |. P.spaces
        |= expr


app : Expr -> Parser Expr
app previous =
    P.succeed (E_App previous)
        |= parenthesized args


tag : Parser Expr
tag =
    P.succeed E_Tag
        |. P.symbol "#"
        |= P.oneOf
            [ P.symbol "_" |> P.map (always True)
            , P.succeed False
            ]
        |= lname
        |= parenthesized construct


error : Parser Expr
error =
    P.succeed E_Error
        |. P.symbol "?"
        |= lname


struct : Parser Expr
struct =
    parenthesized construct
        |> P.map E_Struct


args : Parser E_Args
args =
    structElems (Just ())


construct : Parser E_Construct
construct =
    structElems Nothing


structElems : Maybe arg -> Parser (List (E_StructElem arg))
structElems canBeVar =
    [ P.succeed (::)
        |= structElem canBeVar
        |= ((\acc ->
                [ P.succeed (\e -> e :: acc |> P.Loop)
                    |. P.spaces
                    |. P.symbol ","
                    |. P.spaces
                    |= structElem canBeVar
                , P.succeed (List.reverse acc |> P.Done)
                ]
                    |> P.oneOf
            )
                |> P.loop []
           )
    , P.succeed []
    ]
        |> P.oneOf


structElem : Maybe arg -> Parser (E_StructElem arg)
structElem canBeVar =
    let
        startWithTilde =
            Maybe.map
                (\yes ->
                    P.succeed identity
                        |. P.symbol "~"
                        |= P.oneOf
                            [ P.succeed (\lname_ -> E_Var yes lname_ Simple)
                                |= lname
                            , P.succeed (\lname_ -> E_Var yes lname_ (Field Nothing))
                                |. P.symbol "."
                                |= lname
                            ]
                )
                canBeVar

        startWithDot =
            let
                mutableField =
                    Maybe.map
                        (\yes ->
                            P.succeed (\lname_ -> E_Var yes lname_ (Field Nothing))
                                |. P.symbol "~"
                                |= lname
                        )
                        canBeVar
            in
            P.succeed identity
                |. P.symbol "."
                |= oneOfMaybe
                    [ mutableField
                    , P.andThen startWithField lname |> Just
                    , P.succeed E_KeyVal
                        |. P.symbol "["
                        |. P.spaces
                        |= expr
                        |. P.spaces
                        |. P.symbol "]"
                        |. P.spaces
                        |. P.symbol "="
                        |. P.spaces
                        |= expr
                        |> Just
                    , P.succeed E_Extension
                        |. P.symbol "."
                        |= P.oneOf
                            [ exprBlock |> P.map Just
                            , P.succeed Nothing
                            ]
                        |> Just
                    ]

        startWithField lname_ =
            let
                mutableField =
                    Maybe.map
                        (\yes ->
                            P.succeed (\lname2_ -> E_Var yes lname_ (Field (Just lname2_)))
                                |. P.symbol "~"
                                |= lname
                        )
                        canBeVar
            in
            [ P.succeed identity
                |. P.spaces
                |. P.symbol "="
                |. P.spaces
                |= oneOfMaybe
                    [ mutableField
                    , P.map (Just >> E_Field lname_) expr |> Just
                    ]
            , P.succeed (E_Field lname_ Nothing)
            ]
                |> P.oneOf
    in
    [ startWithTilde
    , Just startWithDot
    , P.map E_Expr expr |> Just
    ]
        |> oneOfMaybe



-- Utils --


isSpace c =
    List.member c [ ' ', '\n', '\t', '\u{000D}', '\u{000B}', '\u{0008}', '\u{000C}' ]


parenthesized parser =
    P.succeed identity
        |. P.symbol "("
        |. P.spaces
        |= parser
        |. P.spaces
        |. P.symbol ")"


oneOfMaybe =
    List.filterMap identity
        >> P.oneOf
