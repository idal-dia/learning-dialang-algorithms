module Test.Parser.SimpleExpr exposing (..)

import Dict
import Expect
import Fuzz
import Parser as P
import Parser.SimpleExpr exposing (..)
import Test exposing (Test)


suite : Test
suite =
    [ Test.test "Local val" <|
        \_ ->
            Expect.equal
                (P.run expr "hello_world_23")
                (Ok (E_Val Local "hello_world_23"))
    , Test.test "Ghost val" <|
        \_ ->
            Expect.equal
                (P.run expr "_booyah")
                (Ok (E_Val Ghost "booyah"))
    , Test.test "Imported val" <|
        \_ ->
            Expect.equal
                (P.run expr "MyModule.hello_world")
                (Ok (E_Val (Import ( "MyModule", [] )) "hello_world"))
    , Test.test "Function application" <|
        \_ ->
            Expect.equal
                (P.run expr "MyModule.hello_world(john, .time=12, ~.greet_count)")
                (Ok (E_App (E_Val (Import ( "MyModule", [] )) "hello_world") [ E_Expr (E_Val Local "john"), E_Field "time" (Just (E_Int 12)), E_Var () "greet_count" (Field Nothing) ]))
    , Test.test "Binop" <|
        \_ ->
            Expect.equal
                (P.run expr "12 + my_var * -14.5")
                (Ok (E_Binop (E_Int 12) "+" (E_Binop (E_Val Local "my_var") "*" (E_Float -14.5))))
    , Test.test "Binop on function applications" <|
        \_ ->
            Expect.equal
                (P.run expr "a(12) + b(.c) * d()")
                (Ok (E_Binop (E_App (E_Val Local "a") [ E_Expr (E_Int 12) ]) "+" (E_Binop (E_App (E_Val Local "b") [ E_Field "c" Nothing ]) "*" (E_App (E_Val Local "d") []))))
    ]
        |> Test.describe "Parser.SimpleExpr"
