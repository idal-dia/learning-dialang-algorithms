module Test.Lexer.Base exposing (..)

import Expect
import Fuzz
import Lexer.Base as L exposing (Token(..))
import Parser as P
import Test exposing (Test)


suite : Test
suite =
    [ Test.test "12 + 8" <|
        \_ ->
            Expect.equal
                (P.run L.parser "12 + 8")
                (Ok [ Int 12, Spacing, Symbol "+", Spacing, Int 8 ])
    , Test.test "124/7*3|> map 74" <|
        \_ ->
            Expect.equal
                (P.run L.parser "124/7*3|> map 74")
                (Ok [ Int 124, Symbol "/", Int 7, Symbol "*", Int 3, Symbol "|>", Spacing, LName False "map", Spacing, Int 74 ])
    , Test.test "find_index" <|
        \_ ->
            let
                find_index_signature =
                    """find_index
  : (a -> Bool), List(a) -> { Int | ?not_found }
"""

                tokens =
                    [ LName False "find_index"
                    , Spacing
                    , Symbol ":"
                    , Spacing
                    , Symbol "("
                    , LName False "a"
                    , Spacing
                    , Symbol "->"
                    , Spacing
                    , UName False "Bool"
                    , Symbol "),"
                    , Spacing
                    , UName False "List"
                    , Symbol "("
                    , LName False "a"
                    , Symbol ")"
                    , Spacing
                    , Symbol "->"
                    , Spacing
                    , Symbol "{"
                    , Spacing
                    , UName False "Int"
                    , Spacing
                    , Symbol "|"
                    , Spacing
                    , Symbol "?"
                    , LName False "not_found"
                    , Spacing
                    , Symbol "}"
                    , Spacing
                    ]
            in
            Expect.equal
                (P.run L.parser find_index_signature)
                (Ok tokens)
    ]
        |> Test.describe "Lexer.Base"
