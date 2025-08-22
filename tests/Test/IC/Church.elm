module Test.IC.Church exposing (..)

import Expect
import Fuzz
import IC.Church exposing (..)
import Test exposing (Test)


suite : Test
suite =
    [ Test.fuzz
        (Fuzz.intRange 0 100)
        "Encode-Decode Nat"
        (\i ->
            Expect.equal
                (nat i |> decodeNat)
                (Just i)
        )
    , Test.fuzz
        Fuzz.bool
        "Encode-Decode Bool"
        (\b ->
            Expect.equal
                (bool b initWG
                    |> decodeBool
                )
                (Just b)
        )
    , Test.fuzz2
        (Fuzz.intRange 0 100)
        (Fuzz.intRange 0 100)
        "Apply sumLam"
        (\i j ->
            let
                sum =
                    sumLam initWG
            in
            Expect.equal
                (apply2 sum (nat i) (nat j)
                    |> reduce { verbose = False }
                    |> decodeNat
                )
                (Just (i + j))
        )
    , Test.fuzz2
        -- Goes up to 8 to avoid "Maximum call stack size exceeded" as Church multiplication is very inneficient
        (Fuzz.intRange 0 8)
        (Fuzz.intRange 0 8)
        "Apply productLam"
        (\i j ->
            let
                product =
                    productLam initWG
            in
            Expect.equal
                (apply2 product (nat i) (nat j)
                    |> reduce { verbose = False }
                    |> decodeNat
                )
                (Just (i * j))
        )

    -- this produces dups facing one an other which is a limit of IC
    , Test.fuzz2
        (Fuzz.pair (Fuzz.intRange 0 4) (Fuzz.intRange 0 4))
        (Fuzz.pair (Fuzz.intRange 0 4) (Fuzz.intRange 0 4))
        "Sum and product composition"
        (\( i, j ) ( k, l ) ->
            case
                [ productLam, sumLam, sumLam ]
                    |> chain initWG
            of
                [ product, sum, sum_ ] ->
                    let
                        sumA =
                            apply2 sum (nat i) (nat j)

                        sumB =
                            apply2 { sum_ | wg = sumA.wg } (nat k) (nat l)
                    in
                    Expect.equal
                        (apply2 product sumA sumB
                            |> reduce { verbose = False }
                            |> decodeNat
                        )
                        (Just ((i + j) * (k + l)))

                _ ->
                    Debug.todo "unreachable"
        )
    , Test.test "Not complete truth table" <|
        \_ ->
            let
                notTruthTable =
                    [ ( True, Just False )
                    , ( False, Just True )
                    ]

                not =
                    notLam initWG

                run =
                    List.map
                        (\( b, _ ) ->
                            apply not (bool b not.wg)
                                |> reduce { verbose = False }
                                |> decodeBool
                                |> Tuple.pair b
                        )
                        notTruthTable
            in
            Expect.equalLists run notTruthTable
    , Test.test "And complete truth table" <|
        \_ ->
            let
                andTruthTable =
                    [ ( True, True, Just True )
                    , ( True, False, Just False )
                    , ( False, True, Just False )
                    , ( False, False, Just False )
                    ]

                and =
                    andLam initWG

                run =
                    List.map
                        (\( a, b, _ ) ->
                            let
                                a_ =
                                    bool a and.wg
                            in
                            apply2 and a_ (bool b a_.wg)
                                |> reduce { verbose = False }
                                |> decodeBool
                                |> (\out -> ( a, b, out ))
                        )
                        andTruthTable
            in
            Expect.equalLists run andTruthTable
    , Test.test "Or complete truth table" <|
        \_ ->
            let
                orTruthTable =
                    [ ( True, True, Just True )
                    , ( True, False, Just True )
                    , ( False, True, Just True )
                    , ( False, False, Just False )
                    ]

                or =
                    orLam initWG

                run =
                    List.map
                        (\( a, b, _ ) ->
                            let
                                a_ =
                                    bool a or.wg
                            in
                            apply2 or a_ (bool b a_.wg)
                                |> reduce { verbose = False }
                                |> decodeBool
                                |> (\out -> ( a, b, out ))
                        )
                        orTruthTable
            in
            Expect.equalLists run orTruthTable
    ]
        |> Test.describe "IChurch"
