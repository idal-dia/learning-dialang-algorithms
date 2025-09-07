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
    term False


exprBlock : Parser Expr
exprBlock =
    term True


term : Bool -> Parser Expr
term isBlock =
    P.oneOf
        [ val
        , num
        , placeholder
        , error
        , P.lazy (\_ -> tag)
        , P.lazy (\_ -> struct)
        ]
        |> P.andThen
            (\previous ->
                P.lazy (\_ -> app previous)
                    :: (if isBlock then
                            []

                        else
                            [ P.lazy (\_ -> binop previous) ]
                       )
                    ++ [ P.succeed previous ]
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
        num_ isNegative =
            let
                neg =
                    if isNegative then
                        negate

                    else
                        identity
            in
            P.number
                { int = Just (neg >> E_Int)
                , hex = Nothing
                , octal = Nothing
                , binary = Nothing
                , float = Just (neg >> E_Float)
                }
    in
    P.oneOf
        [ P.succeed identity
            |. P.symbol "-"
            |= num_ True
        , num_ False
        ]


placeholder : Parser Expr
placeholder =
    P.symbol "$"
        |> P.map (always E_Placeholder)


binop : Expr -> Parser Expr
binop previous =
    let
        isValidBinopChar c =
            not (isSpace c || Char.isAlphaNum c)
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
    P.succeed (::)
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


structElem : Maybe arg -> Parser (E_StructElem arg)
structElem canBeVar =
    let
        var =
            case canBeVar of
                Nothing ->
                    []

                Just yes ->
                    [ P.succeed (\lname_ -> E_Var yes lname_ Simple)
                        |. P.symbol "~"
                        |= lname
                    , P.succeed (\lname_ -> E_Var yes lname_ (Field Nothing))
                        |. P.symbol "~."
                        |= lname
                    , P.succeed (\lname_ lname2_ -> E_Var yes lname_ (Field (Just lname2_)))
                        |. P.symbol "."
                        |= lname
                        |. P.spaces
                        |. P.symbol "="
                        |. P.spaces
                        |. P.symbol "~"
                        |= lname
                    ]
    in
    (var
        ++ [ expr |> P.map E_Expr
           , P.succeed E_Field
                |. P.symbol "."
                |= lname
                |. P.spaces
                |. P.symbol "="
                |. P.spaces
                |= (expr |> P.map Just)
           , P.succeed (\lname_ -> E_Field lname_ Nothing)
                |. P.symbol "."
                |= lname
           , P.succeed E_KeyVal
                |. P.symbol ".["
                |. P.spaces
                |= expr
                |. P.spaces
                |. P.symbol "]"
                |. P.spaces
                |. P.symbol "="
                |. P.spaces
                |= expr
           , P.succeed E_Extension
                |. P.symbol ".."
                |= P.oneOf
                    [ exprBlock |> P.map Just
                    , P.succeed Nothing
                    ]
           ]
    )
        |> P.oneOf



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
