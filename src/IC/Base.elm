module IC.Base exposing (..)

{-| This basic implementation is based on [this article](https://zicklag.katharos.group/blog/interaction-nets-combinators-calculus/).
[This conference](https://www.youtube.com/watch?v=F880mnxu9c0) has been helpful as well.
-}


type Tree
    = Dup Tree Tree -- duplicator (δ)
    | Cons Tree Tree -- constructor (γ)
    | Era -- erasor (ε)
    | Wire Int


type alias Net =
    { root : Tree
    , redexes : List Redex
    , wg : WireGenerator -- to get unique keys for wires
    }


type alias Redex =
    -- interaction (triangles facing one an other)
    ( Tree, Tree )



-- Reduce --


reduce : Net -> Tree
reduce t =
    let
        annihilation x y otherRedexes =
            { t | redexes = ( x.a, y.a ) :: ( x.b, y.b ) :: otherRedexes }

        duplication wg cons dup otherRedexes =
            let
                ( w1, wg1 ) =
                    runWG wg

                ( w2, wg2 ) =
                    runWG wg1

                ( w3, wg3 ) =
                    runWG wg2

                ( w4, wg4 ) =
                    runWG wg3
            in
            { t
                | redexes =
                    ( Dup w1 w2, cons.a )
                        :: ( Dup w3 w4, cons.b )
                        :: ( Cons w3 w1, dup.a )
                        :: ( Cons w4 w2, dup.b )
                        :: otherRedexes
                , wg = wg4
            }

        erasure node otherRedexes =
            let
                listWires n =
                    case n of
                        Dup a b ->
                            listWires a ++ listWires b

                        Cons a b ->
                            listWires a ++ listWires b

                        Wire wireRef ->
                            [ wireRef ]

                        Era ->
                            []
            in
            { t
                | redexes =
                    List.map
                        (Wire >> Tuple.pair Era)
                        (listWires node)
                        ++ otherRedexes
            }

        connectWire wire otherRedexes =
            { t
                | redexes =
                    List.map
                        (Tuple.mapBoth
                            (connectWireInTree wire)
                            (connectWireInTree wire)
                        )
                        otherRedexes
                , root = connectWireInTree wire t.root
            }

        connectWireInTree : ( Tree, Int ) -> Tree -> Tree
        connectWireInTree ( insert, atWireRef ) into =
            case into of
                Dup a b ->
                    Dup
                        (connectWireInTree ( insert, atWireRef ) a)
                        (connectWireInTree ( insert, atWireRef ) b)

                Cons a b ->
                    Cons
                        (connectWireInTree ( insert, atWireRef ) a)
                        (connectWireInTree ( insert, atWireRef ) b)

                Wire i ->
                    if i == atWireRef then
                        insert

                    else
                        Wire i

                Era ->
                    Era
    in
    case t.redexes of
        ( Dup xa xb, Dup ya yb ) :: queue ->
            annihilation { a = xa, b = xb } { a = ya, b = yb } queue
                |> reduce

        ( Cons xa xb, Cons ya yb ) :: queue ->
            annihilation { a = xa, b = xb } { a = ya, b = yb } queue
                |> reduce

        ( Cons ca cb, Dup da db ) :: queue ->
            duplication t.wg { a = ca, b = cb } { a = da, b = db } queue
                |> reduce

        ( Dup da db, Cons ca cb ) :: queue ->
            duplication t.wg { a = ca, b = cb } { a = da, b = db } queue
                |> reduce

        ( Wire i, Wire j ) :: queue ->
            if i == j then
                { t | redexes = queue }
                    |> reduce

            else
                connectWire ( Wire i, j ) queue
                    |> reduce

        ( n, Wire i ) :: queue ->
            connectWire ( n, i ) queue
                |> reduce

        ( Wire i, n ) :: queue ->
            connectWire ( n, i ) queue
                |> reduce

        ( Era, Era ) :: queue ->
            { t | redexes = queue }
                |> reduce

        ( Era, n ) :: queue ->
            erasure n queue
                |> reduce

        ( n, Era ) :: queue ->
            erasure n queue
                |> reduce

        [] ->
            t.root



-- Examples --


{-| λx.x
-}
identity : WireGenerator -> Net
identity wg =
    let
        ( w, wg1 ) =
            runWG wg
    in
    { root = Cons w w, redexes = [], wg = wg1 }


{-| λx.x(x)
-}
applyOnSelf : WireGenerator -> Net
applyOnSelf wg =
    let
        ( wReturn, wg1 ) =
            runWG wg

        ( wArg, wg2 ) =
            runWG wg1
    in
    { root = Cons (Dup wArg (Cons wArg wReturn)) wReturn, redexes = [], wg = wg2 }


{-| Returns the absolute value of the given `Int` as a Church numeral.
-}
number : WireGenerator -> Int -> Net
number wg num =
    let
        constructDup n ( w0_, wg0_ ) =
            case n of
                0 ->
                    ( Era, ( w0_, wg0_ ) )

                1 ->
                    let
                        ( w1, wg1 ) =
                            runWG wg0_
                    in
                    ( Cons w0_ w1, ( w1, wg1 ) )

                2 ->
                    let
                        ( w1, wg1 ) =
                            runWG wg0_

                        ( w2, wg2 ) =
                            runWG wg1
                    in
                    ( Dup (Cons w0_ w1) (Cons w1 w2), ( w2, wg2 ) )

                _ ->
                    let
                        ( w1, wg1 ) =
                            runWG wg0_
                    in
                    constructDup (n - 1) ( w1, wg1 )
                        |> Tuple.mapFirst (Dup (Cons w0_ w1))

        ( w0, wg0 ) =
            runWG wg

        ( dupTrees, ( lastWire, lastWG ) ) =
            constructDup (abs num) ( w0, wg0 )
    in
    -- zero = ( Cons Era (Cons w0 w0) )
    -- one = ( Cons (Cons w0 w1) (Cons w0 w1) )
    -- two = ( Cons (Dup (Cons w0 w1) (Cons w1 w2)) (Cons w0 w2) )
    -- three = ( Cons (Dup (Cons w0 w1) (Dup (Cons w1 w2) (Cons w2 w3))) (Cons w0 w3) )
    { root = Cons dupTrees (Cons w0 lastWire)
    , redexes = []
    , wg = lastWG
    }


{-| λa.λb.a+b

This implementation has some issues and isn't able to clear the tests.

Some extra Church numerals operations could be implemented with the help of [this article](https://yaossg.com/site/docs/lambda/3).

-}
sum : WireGenerator -> Net
sum wg =
    let
        ( w0, wg0 ) =
            runWG wg

        ( w1, wg1 ) =
            runWG wg0

        ( w1_, wg2 ) =
            runWG wg1

        ( wA, wg3 ) =
            runWG wg2

        ( wB, wg4 ) =
            runWG wg3

        ( wReturn, wg5 ) =
            runWG wg4

        ( wPass, wg6 ) =
            runWG wg5
    in
    { root = Cons wA (Cons wB (Cons (Dup w1 w1_) (Cons w0 wReturn)))
    , redexes =
        [ ( wA, Cons w1 (Cons w0 wPass) )
        , ( wB, Cons w1_ (Cons wPass wReturn) )
        ]
    , wg = wg6
    }


{-| λt.λf.t
-}
true : WireGenerator -> Net
true wg =
    let
        ( wt, wgt ) =
            runWG wg
    in
    treeToNet ( Cons wt (Cons Era wt), wgt )


{-| λt.λf.f
-}
false : WireGenerator -> Net
false wg =
    let
        ( wf, wgf ) =
            runWG wg
    in
    treeToNet ( Cons Era (Cons wf wf), wgf )


bool : WireGenerator -> Bool -> Net
bool wg b =
    if b then
        true wg

    else
        false wg


{-| λx.λy.(x (y true false) false)
-}
and : WireGenerator -> Net
and wg =
    let
        x =
            runWG wg |> treeToNet

        wx =
            x.root

        y =
            runWG x.wg |> treeToNet

        wy =
            y.root

        f =
            false y.wg

        f_ =
            false f.wg

        t =
            true f_.wg
    in
    apply2 x (apply2 y t f) f_
        |> (\net -> { net | root = Cons wx (Cons wy net.root) })


{-| λx.λy.(x true (y true false))
-}
or : WireGenerator -> Net
or wg =
    let
        x =
            runWG wg |> treeToNet

        wx =
            x.root

        y =
            runWG x.wg |> treeToNet

        wy =
            y.root

        t =
            true y.wg

        t_ =
            true t.wg

        f =
            false t_.wg
    in
    apply2 x t (apply2 y t_ f)
        |> (\net -> { net | root = Cons wx (Cons wy net.root) })


{-| λb.(b false true)
-}
not : WireGenerator -> Net
not wg =
    let
        b =
            runWG wg |> treeToNet

        wb =
            b.root

        t =
            true b.wg

        f =
            false t.wg
    in
    apply2 b f t
        |> (\net -> { net | root = Cons wb net.root })


era : Net
era =
    { root = Era, redexes = [], wg = WireGenerator 0 }


{-| f(a)
-}
apply : Net -> Net -> Net
apply f a =
    let
        wg =
            let
                ( WireGenerator i, WireGenerator j ) =
                    ( f.wg, a.wg )
            in
            WireGenerator (max i j)

        ( w, wg1 ) =
            runWG wg
    in
    { root = w
    , redexes =
        ( f.root, Cons a.root w )
            :: f.redexes
            ++ a.redexes
    , wg = wg1
    }


{-| f(a, b)
-}
apply2 : Net -> Net -> Net -> Net
apply2 f a b =
    let
        wg =
            let
                ( WireGenerator i, WireGenerator j, WireGenerator k ) =
                    ( f.wg, a.wg, b.wg )
            in
            WireGenerator (max (max i j) k)

        ( w, wg1 ) =
            runWG wg
    in
    { root = w
    , redexes =
        ( f.root, Cons a.root (Cons b.root w) )
            :: f.redexes
            ++ a.redexes
            ++ b.redexes
    , wg = wg1
    }



-- Helpers --


{-| Used to ensure wire references are unique
-}
type WireGenerator
    = WireGenerator Int


initWG : WireGenerator
initWG =
    WireGenerator 0


runWG : WireGenerator -> ( Tree, WireGenerator )
runWG (WireGenerator i) =
    ( Wire i, WireGenerator (i + 1) )


treeToNet : ( Tree, WireGenerator ) -> Net
treeToNet ( n, wg ) =
    { root = n, redexes = [], wg = wg }


isTrue : Tree -> Bool
isTrue t =
    case t of
        Cons (Wire wt) (Cons Era (Wire wt_)) ->
            wt == wt_

        _ ->
            False


isFalse : Tree -> Bool
isFalse t =
    case t of
        Cons Era (Cons (Wire wf) (Wire wf_)) ->
            wf == wf_

        _ ->
            False


type Bool_
    = True_
    | False_
    | Other_


toBool : Tree -> Bool_
toBool t =
    if isTrue t then
        True_

    else if isFalse t then
        False_

    else
        Other_


toNumber : Tree -> Maybe Int
toNumber node =
    let
        destructDup : Tree -> Int -> Maybe { lastWire : Int, count : Int }
        destructDup n w0 =
            case n of
                Era ->
                    Just { lastWire = w0, count = 0 }

                Cons (Wire w0_) (Wire w1) ->
                    if w0 == w0_ then
                        Just { lastWire = w1, count = 1 }

                    else
                        Nothing

                Dup (Cons (Wire w0_) (Wire w1)) (Cons (Wire w1_) (Wire w2)) ->
                    if w0 == w0_ && w1 == w1_ then
                        Just { lastWire = w2, count = 2 }

                    else
                        Nothing

                Dup (Cons (Wire w0_) (Wire w1)) queue ->
                    if w0 == w0_ then
                        Maybe.map
                            (\r -> { r | count = r.count + 1 })
                            (destructDup queue w1)

                    else
                        Nothing

                _ ->
                    Nothing
    in
    case node of
        Cons dupTrees (Cons (Wire w0) (Wire lastWire_)) ->
            destructDup dupTrees w0
                |> Maybe.andThen
                    (\{ lastWire, count } ->
                        if lastWire == lastWire_ then
                            Just count

                        else
                            Nothing
                    )

        _ ->
            Nothing
