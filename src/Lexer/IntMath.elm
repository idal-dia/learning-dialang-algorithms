module Lexer.IntMath exposing (..)

import Parser as P exposing ((|.), (|=), Parser)


type Token
    = Int Int
    | Op String


parser : Parser (List Token)
parser =
    P.loop
        []
        (\acc ->
            P.succeed identity
                |. P.spaces
                |= P.oneOf
                    [ P.int |> P.map Int
                    , P.chompWhile
                        (\c -> c /= ' ' && c /= '\n' && c /= '\t' && not (Char.isDigit c))
                        |> P.getChompedString
                        |> P.map Op
                    ]
                |> P.map
                    (\token ->
                        case token of
                            Op "" ->
                                P.Done (List.reverse acc)

                            _ ->
                                P.Loop (token :: acc)
                    )
        )
