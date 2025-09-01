module Lexer.Base exposing (IsGhost, Token(..), parser)

{-| This lexer forms generic tokens that might be used for a basic implementation of Dialang's syntax.
-}

import Parser as P exposing ((|.), (|=), Parser)


type Token
    = LName IsGhost String
    | UName IsGhost String
    | Int Int
    | Float Float
    | Symbol String
    | Spacing


type alias IsGhost =
    Bool


name : Parser Token
name =
    let
        toTokenConstructor c =
            if
                String.toList c
                    |> List.head
                    |> Maybe.map Char.isLower
                    |> Maybe.withDefault False
            then
                ( LName, c )

            else
                ( UName, c )
    in
    P.succeed
        (\isGhost ( toToken, head ) body ->
            toToken isGhost (head ++ body)
        )
        |= P.oneOf
            [ P.chompIf ((==) '_') |> P.map (always True)
            , P.succeed False
            ]
        |= (P.chompIf Char.isAlpha |> P.getChompedString |> P.map toTokenConstructor)
        |= (P.chompWhile (\c -> Char.isAlphaNum c || c == '_') |> P.getChompedString)


num : Parser Token
num =
    P.number
        { int = Just Int
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just Float
        }


isSpace c =
    List.member c [ ' ', '\n', '\t', '\u{000D}', '\u{000B}', '\u{0008}', '\u{000C}' ]


spacing : Parser Token
spacing =
    P.succeed Spacing
        |. P.chompIf isSpace
        |. P.chompWhile isSpace


symbol : Parser Token
symbol =
    let
        isValidSymbolChar c =
            not (isSpace c || Char.isAlphaNum c)
    in
    (P.succeed ()
        |. P.chompIf isValidSymbolChar
        |. P.chompWhile isValidSymbolChar
    )
        |> P.getChompedString
        |> P.map Symbol


parser : Parser (List Token)
parser =
    (\acc ->
        let
            loop =
                P.map (\token -> P.Loop (token :: acc))

            done =
                P.succeed (P.Done (List.reverse acc))
        in
        P.oneOf
            [ name |> loop
            , num |> loop
            , spacing |> loop
            , symbol |> loop
            , done
            ]
    )
        |> P.loop []
