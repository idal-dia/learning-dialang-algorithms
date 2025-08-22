module Test.Parser.ShuntingYard exposing (..)

import Dict
import Expect
import Fuzz
import Parser as P
import Parser.ShuntingYard as S exposing (Assoc(..), Tree(..))
import Test exposing (Test)


suite : Test
suite =
    [ Test.test "12 + 8" <|
        \_ ->
            let
                opTable =
                    { prec = Dict.singleton "+" 1
                    , assoc = Dict.singleton 1 Left
                    }
            in
            Expect.equal
                (P.run (S.parser opTable) "12 + 8")
                (Ok (Op "+" (Int 12) (Int 8)))
    , Test.test "124/7*3^74^2+4" <|
        \_ ->
            let
                opTable =
                    { prec =
                        [ ( "+", 1 )
                        , ( "/", 2 )
                        , ( "*", 2 )
                        , ( "^", 3 )
                        ]
                            |> Dict.fromList
                    , assoc =
                        [ ( 1, Left )
                        , ( 2, Left )
                        , ( 3, Right )
                        ]
                            |> Dict.fromList
                    }
            in
            Expect.equal
                (P.run (S.parser opTable) "124/7*3^74^2+4")
                (Op "+"
                    (Op "*"
                        (Op "/"
                            (Int 124)
                            (Int 7)
                        )
                        (Op "^"
                            (Int 3)
                            (Op "^"
                                (Int 74)
                                (Int 2)
                            )
                        )
                    )
                    (Int 4)
                    |> Ok
                )
    ]
        |> Test.describe "Parser.ShuntingYard"
