module Test.Lexer.IntMath exposing (..)

import Expect
import Fuzz
import Lexer.IntMath as L exposing (Token(..))
import Parser as P
import Test exposing (Test)


suite : Test
suite =
    [ Test.test "12 + 8" <|
        \_ ->
            Expect.equal
                (P.run L.parser "12 + 8")
                (Ok [ Int 12, Op "+", Int 8 ])
    , Test.test "124/7*3|> map 74" <|
        \_ ->
            Expect.equal
                (P.run L.parser "124/7*3|> map 74")
                (Ok [ Int 124, Op "/", Int 7, Op "*", Int 3, Op "|>", Op "map", Int 74 ])
    ]
        |> Test.describe "Lexer.IntMath"
