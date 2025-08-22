module IC.Church exposing (..)

{-| This IC implementation builds upon the `Base.elm` one, adds Church numerals andFunc some operators (`+` and `*`) to be applied on them.

It follows [this simple article](https://t6.fyi/guides/inets).

-}


type Tree
    = Sum Tree Tree
    | Product Tree Tree
    | Incr Tree
    | Zero
    | Dup Tree Tree -- duplicator (δ)
    | Cons Tree Tree -- constructor (γ)
    | Era -- erasor (ε)
    | Wire Int


type alias Net =
    { root : Tree
    , redexes : List Redex
    , wg : WireGenerator
    }


type alias Redex =
    -- interaction (triangles facing one an other)
    ( Tree, Tree )


reduce : { verbose : Bool } -> Net -> Net
reduce option net =
    let
        wg =
            combineWG [ net ]

        _ =
            if option.verbose then
                Debug.log "net" net

            else
                net
    in
    case net.redexes of
        ( Sum sa sb, Incr i ) :: queue ->
            let
                ( w, wg1 ) =
                    runWG net.wg
            in
            { root = net.root
            , redexes =
                ( Sum sa w, i )
                    :: ( sb, Incr w )
                    :: queue
            , wg = wg1
            }
                |> reduce option

        ( Sum sa sb, Zero ) :: queue ->
            { net | redexes = ( sa, sb ) :: queue }
                |> reduce option

        ( Dup da db, Zero ) :: queue ->
            { net | redexes = ( da, Zero ) :: ( db, Zero ) :: queue }
                |> reduce option

        ( Dup da db, Incr i ) :: queue ->
            let
                ( ia, ib, wg1 ) =
                    runWG2 net.wg
            in
            { root = net.root
            , redexes =
                ( Dup ia ib, i )
                    :: ( da, Incr ia )
                    :: ( db, Incr ib )
                    :: queue
            , wg = wg1
            }
                |> reduce option

        ( Dup da db, Cons ca cb ) :: queue ->
            -- duplication
            let
                ( w1, w2, wg1 ) =
                    runWG2 net.wg

                ( w3, w4, wg2 ) =
                    runWG2 wg1
            in
            { root = net.root
            , redexes =
                ( Dup w1 w2, ca )
                    :: ( Dup w3 w4, cb )
                    :: ( Cons w3 w1, da )
                    :: ( Cons w4 w2, db )
                    :: queue
            , wg = wg2
            }
                |> reduce option

        ( Cons a b, Cons c d ) :: queue ->
            -- annihilation
            { net | redexes = ( a, c ) :: ( b, d ) :: queue }
                |> reduce option

        ( Dup a b, Dup c d ) :: queue ->
            -- annihilation
            -- Debug.log "Dups facing on an other" net
            { net | redexes = ( a, c ) :: ( b, d ) :: queue }
                |> reduce option

        ( Product pa pb, Zero ) :: queue ->
            { net | redexes = ( Era, pa ) :: ( pb, Zero ) :: queue }
                |> reduce option

        ( Product pa pb, Incr i ) :: queue ->
            let
                ( dpa1, dpa2, wg1 ) =
                    runWG2 net.wg
            in
            { root = net.root
            , redexes =
                queue
                    ++ [ ( Product dpa2 (Sum dpa1 pb), i )
                       , ( Dup dpa1 dpa2, pa )
                       ]
            , wg = wg1
            }
                |> reduce option

        ( t, Wire w ) :: queue ->
            { root = connectWire t w net.root
            , redexes =
                List.map
                    (Tuple.mapBoth
                        (connectWire t w)
                        (connectWire t w)
                    )
                    queue
            , wg = net.wg
            }
                |> reduce option

        ( Era, t ) :: queue ->
            { net
                | redexes =
                    List.map
                        (Wire >> Tuple.pair Era)
                        (listWires t)
                        ++ queue
            }
                |> reduce option

        ( Zero, Zero ) :: queue ->
            { net | redexes = queue }
                |> reduce option

        ( t1, t2 ) :: queue ->
            { net | redexes = ( t2, t1 ) :: queue }
                |> reduce option

        [] ->
            net


connectWire : Tree -> Int -> Tree -> Tree
connectWire insert atWireRef into =
    case into of
        Sum a b ->
            Sum
                (connectWire insert atWireRef a)
                (connectWire insert atWireRef b)

        Incr i ->
            Incr (connectWire insert atWireRef i)

        Zero ->
            Zero

        Dup a b ->
            Dup
                (connectWire insert atWireRef a)
                (connectWire insert atWireRef b)

        Cons a b ->
            Cons
                (connectWire insert atWireRef a)
                (connectWire insert atWireRef b)

        Era ->
            Era

        Product a b ->
            Product
                (connectWire insert atWireRef a)
                (connectWire insert atWireRef b)

        Wire w ->
            if w == atWireRef then
                insert

            else
                Wire w


listWires : Tree -> List Int
listWires tree =
    case tree of
        Sum a b ->
            listWires a ++ listWires b

        Product a b ->
            listWires a ++ listWires b

        Incr a ->
            listWires a

        Zero ->
            []

        Dup a b ->
            listWires a ++ listWires b

        Cons a b ->
            listWires a ++ listWires b

        Wire wireRef ->
            [ wireRef ]

        Era ->
            []



-- Encode-Decode --


{-| Encode absolute value of the given `Int` in Church numeral form.

    0 := Zero

    3 := Incr (Incr (Incr Zero))

-}
nat : Int -> Net
nat i_ =
    let
        rec i acc =
            if i == 0 then
                acc

            else
                rec (i - 1) (Incr acc)
    in
    { root = rec (abs i_) Zero
    , redexes = []
    , wg = initWG
    }


decodeNat : Net -> Maybe Int
decodeNat net =
    let
        rec t acc =
            case t of
                Incr subTree ->
                    rec subTree (acc + 1)

                Zero ->
                    Just acc

                _ ->
                    Nothing
    in
    rec net.root 0


{-|

    True := λt.λf.t

    False := λt.λf.f

-}
bool : Bool -> WireGenerator -> Net
bool b wg =
    let
        ( w, wg1 ) =
            runWG wg
    in
    { root =
        if b then
            Cons w (Cons Era w)

        else
            Cons Era (Cons w w)
    , redexes = []
    , wg = wg1
    }


decodeBool : Net -> Maybe Bool
decodeBool net =
    case net.root of
        Cons (Wire wt) (Cons Era (Wire wt_)) ->
            if wt == wt_ then
                Just True

            else
                Nothing

        Cons Era (Cons (Wire wf) (Wire wf_)) ->
            if wf == wf_ then
                Just False

            else
                Nothing

        _ ->
            Nothing



-- Lambdas --


sumLam : WireGenerator -> Net
sumLam wg =
    case
        [ wire, wire, wire ]
            |> chain wg
    of
        [ a, b, ret ] ->
            cons a (cons b ret)
                |> insertRedex ( sum a ret, b )

        _ ->
            Debug.todo "unreachable"


productLam : WireGenerator -> Net
productLam wg =
    case
        [ wire, wire, wire ]
            |> chain wg
    of
        [ a, b, ret ] ->
            cons a (cons b ret)
                |> insertRedex ( product a ret, b )

        _ ->
            Debug.todo "unreachable"


{-| λx.λy.(x (y true false) false)
-}
andLam : WireGenerator -> Net
andLam wg =
    case
        [ wire
        , wire
        , bool False
        , bool False
        , bool True
        ]
            |> chain wg
    of
        [ x, y, f, f_, t ] ->
            cons x (cons y (apply2 x (apply2 y t f) f_))

        _ ->
            Debug.todo "unreachable"


{-| λx.λy.(x true (y true false))
-}
orLam : WireGenerator -> Net
orLam wg =
    case
        [ wire
        , wire
        , bool True
        , bool True
        , bool False
        ]
            |> chain wg
    of
        [ x, y, t, t_, f ] ->
            cons x (cons y (apply2 x t (apply2 y t_ f)))

        _ ->
            Debug.todo "unreachable"


{-| λb.(b false true)
-}
notLam : WireGenerator -> Net
notLam wg =
    case
        chain wg [ wire, bool True, bool False ]
    of
        [ b, t, f ] ->
            cons b (apply2 b f t)

        _ ->
            Debug.todo "unreachable"



-- agents as nets


wire : WireGenerator -> Net
wire (WireGenerator i) =
    { root = Wire i
    , redexes = []
    , wg = WireGenerator (i + 1)
    }


cons : Net -> Net -> Net
cons a b =
    { root = Cons a.root b.root
    , redexes = a.redexes ++ b.redexes
    , wg = combineWG [ a, b ]
    }


dup : Net -> Net -> Net
dup a b =
    { root = Dup a.root b.root
    , redexes = a.redexes ++ b.redexes
    , wg = combineWG [ a, b ]
    }


era : Net
era =
    { root = Era, redexes = [], wg = initWG }


sum : Net -> Net -> Net
sum a b =
    { root = Sum a.root b.root
    , redexes = a.redexes ++ b.redexes
    , wg = combineWG [ a, b ]
    }


product : Net -> Net -> Net
product a b =
    { root = Product a.root b.root
    , redexes = a.redexes ++ b.redexes
    , wg = combineWG [ a, b ]
    }


zero : Net
zero =
    { root = Zero, redexes = [], wg = initWG }


incr : Net -> Net
incr n =
    { n | root = Incr n.root }



-- application


{-| f(a)
-}
apply : Net -> Net -> Net
apply f a =
    let
        wg =
            combineWG [ f, a ]

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
            combineWG [ f, a, b ]

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


insertRedex : ( Net, Net ) -> Net -> Net
insertRedex ( a, b ) net =
    { root = net.root
    , redexes =
        ( a.root, b.root )
            :: a.redexes
            ++ b.redexes
            ++ net.redexes
    , wg = combineWG [ a, b, net ]
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


runWG2 : WireGenerator -> ( Tree, Tree, WireGenerator )
runWG2 (WireGenerator i) =
    ( Wire i
    , Wire (i + 1)
    , WireGenerator (i + 2)
    )


treeToNet : Tree -> Net
treeToNet tree =
    let
        highestWireRef t =
            case t of
                Sum a b ->
                    max (highestWireRef a) (highestWireRef b)

                Incr i ->
                    highestWireRef i

                Zero ->
                    -1

                Dup a b ->
                    max (highestWireRef a) (highestWireRef b)

                Cons a b ->
                    max (highestWireRef a) (highestWireRef b)

                Era ->
                    -1

                Product a b ->
                    max (highestWireRef a) (highestWireRef b)

                Wire w ->
                    w
    in
    { root = tree
    , redexes = []
    , wg =
        (highestWireRef tree + 1)
            |> WireGenerator
    }


combineWG : List Net -> WireGenerator
combineWG nets =
    List.map
        (\{ wg } ->
            let
                (WireGenerator i) =
                    wg
            in
            i
        )
        nets
        |> List.maximum
        |> Maybe.withDefault 0
        |> WireGenerator


chain : WireGenerator -> List (WireGenerator -> Net) -> List Net
chain wg list =
    let
        ( netList, lastWG ) =
            List.foldr
                (\builder ( acc, wg_ ) ->
                    let
                        net =
                            builder wg_
                    in
                    ( net :: acc, net.wg )
                )
                ( [], wg )
                list
    in
    List.map (\n -> { n | wg = lastWG }) netList
