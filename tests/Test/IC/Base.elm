module Test.IC.Base exposing (..)

import Expect
import Fuzz
import IC.Base as B exposing (Bool_(..), Tree(..))
import Test exposing (Test)


suite : Test
suite =
    [ Test.test "Article's example" <|
        \_ ->
            let
                applyOnSelf =
                    B.applyOnSelf B.initWG

                identity =
                    B.identity applyOnSelf.wg

                run =
                    B.apply applyOnSelf identity
                        |> B.reduce

                return =
                    Cons (Wire 6) (Wire 6)
            in
            Expect.equal run return
    , Test.test "Validate number zero's structure" <|
        \_ ->
            let
                zero =
                    B.number B.initWG 0
                        |> .root

                w0 =
                    Wire 0

                return =
                    Cons Era (Cons w0 w0)
            in
            Expect.equal zero return
    , Test.test "Validate number one's structure" <|
        \_ ->
            let
                one =
                    B.number B.initWG 1
                        |> .root

                w =
                    Wire

                return =
                    Cons
                        (Cons (w 0) (w 1))
                        (Cons (w 0) (w 1))
            in
            Expect.equal one return
    , Test.test "Validate number two's structure" <|
        \_ ->
            let
                two =
                    B.number B.initWG 2
                        |> .root

                w =
                    Wire

                return =
                    Cons
                        (Dup
                            (Cons (w 0) (w 1))
                            (Cons (w 1) (w 2))
                        )
                        (Cons (w 0) (w 2))
            in
            Expect.equal two return
    , Test.test "Validate number three's structure" <|
        \_ ->
            let
                three =
                    B.number B.initWG 3
                        |> .root

                w =
                    Wire

                return =
                    Cons
                        (Dup
                            (Cons (w 0) (w 1))
                            (Dup
                                (Cons (w 1) (w 2))
                                (Cons (w 2) (w 3))
                            )
                        )
                        (Cons (w 0) (w 3))
            in
            Expect.equal three return
    , Test.test "Apply identity to erasure" <|
        \_ ->
            let
                identity =
                    B.identity B.initWG

                run =
                    B.apply identity B.era
                        |> B.reduce

                return =
                    Era
            in
            Expect.equal run return
    , Test.test "Apply applyOnSelf to erasure" <|
        \_ ->
            let
                applyOnSelf =
                    B.applyOnSelf B.initWG

                run =
                    B.apply applyOnSelf B.era
                        |> B.reduce

                return =
                    Era
            in
            Expect.equal run return
    , Test.test "Apply erasure to applyOnSelf" <|
        \_ ->
            let
                applyOnSelf =
                    B.applyOnSelf B.initWG

                run =
                    B.apply B.era applyOnSelf
                        |> B.reduce

                return =
                    Era
            in
            Expect.equal run return
    , Test.test "Apply erasure to identity" <|
        \_ ->
            let
                identity =
                    B.identity B.initWG

                run =
                    B.apply B.era identity
                        |> B.reduce

                return =
                    Era
            in
            Expect.equal run return
    , Test.fuzz (Fuzz.intRange -30 30) "Apply erasure to numbers" applyEraToNumber
    , Test.fuzz (Fuzz.intRange -30 30) "Apply identity to numbers" applyIdentityOnNum
    , Test.fuzz (Fuzz.intRange 0 50) "Encode and decode number" numEncodingDecoding

    -- -- failing test
    -- , Test.fuzz
    --     ( Fuzz.pair
    --         (Fuzz.intRange 0 5)
    --         (Fuzz.intRange 0 5)
    --     )
    --     "Sum numbers"
    --     sumNumbers
    , Test.test "Not complete truth table" <|
        \_ ->
            let
                notTruthTable =
                    [ ( True, False_ )
                    , ( False, True_ )
                    ]

                not =
                    B.not B.initWG

                run =
                    List.map
                        (\( b, _ ) ->
                            B.apply not (B.bool not.wg b)
                                |> B.reduce
                                |> B.toBool
                                |> Tuple.pair b
                        )
                        notTruthTable
            in
            Expect.equalLists run notTruthTable
    , Test.test "And complete truth table" <|
        \_ ->
            let
                andTruthTable =
                    [ ( True, True, True_ )
                    , ( True, False, False_ )
                    , ( False, True, False_ )
                    , ( False, False, False_ )
                    ]

                and =
                    B.and B.initWG

                run =
                    List.map
                        (\( a, b, _ ) ->
                            let
                                a_ =
                                    B.bool and.wg a
                            in
                            B.apply2 and a_ (B.bool a_.wg b)
                                |> B.reduce
                                |> B.toBool
                                |> (\out -> ( a, b, out ))
                        )
                        andTruthTable
            in
            Expect.equalLists run andTruthTable
    , Test.test "Or complete truth table" <|
        \_ ->
            let
                orTruthTable =
                    [ ( True, True, True_ )
                    , ( True, False, True_ )
                    , ( False, True, True_ )
                    , ( False, False, False_ )
                    ]

                or =
                    B.or B.initWG

                run =
                    List.map
                        (\( a, b, _ ) ->
                            let
                                a_ =
                                    B.bool or.wg a
                            in
                            B.apply2 or a_ (B.bool a_.wg b)
                                |> B.reduce
                                |> B.toBool
                                |> (\out -> ( a, b, out ))
                        )
                        orTruthTable
            in
            Expect.equalLists run orTruthTable
    ]
        |> Test.describe "IC.Basic"


applyIdentityOnNum num =
    let
        numIC =
            B.number B.initWG num

        identity =
            B.identity numIC.wg

        run =
            B.apply identity numIC
                |> B.reduce
    in
    Expect.equal run numIC.root


numEncodingDecoding num =
    let
        numIC =
            B.number B.initWG num

        num_ =
            B.toNumber numIC.root
    in
    Expect.equal (Just num) num_


applyEraToNumber n =
    let
        num =
            B.number B.initWG n

        run =
            B.apply B.era num
                |> B.reduce

        return =
            Era
    in
    Expect.equal run return


sumNumbers ( a, b ) =
    let
        a_ =
            B.number B.initWG a

        b_ =
            B.number a_.wg b

        sum_ =
            B.sum b_.wg

        result =
            B.apply sum_ a_
                |> (\t -> B.apply t b_)
                |> B.reduce
                |> Debug.log ("reduced " ++ String.fromInt a ++ "+" ++ String.fromInt b ++ " sum")
                |> B.toNumber
    in
    Expect.equal (Just (a + b)) result
