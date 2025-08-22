module IC.HVM2 exposing (..)

{-| This implementation builds up on the IC.Church for its interface design one and follows the [HVM2 paper](https://github.com/HigherOrderCO/HVM/blob/main/paper/HVM2.pdf)'s architecture. Though it simplifies it by skipping the memory layout and the paralelization parts.

This implementation is work in progress.

-}

import Dict exposing (Dict)
import Tuple3


type Tree
    = Wire WRef -- called var in the paper
    | Nullary Nullary
    | Binary Binary Tree Tree


type Nullary
    = Era
    | Ref String -- reference
    | Num Numeric -- numeric


{-| We simplify the number's encodings as this implementation doesn't have the same optimization constraints.
-}
type Numeric
    = Number Int
    | UnApp (Int -> Int -> Int) -- unaplied operation
    | PartApp (Int -> Int) -- partially applied operation


type Binary
    = Con -- constructor
    | Dup -- duplicator
    | Ope -- operator
    | Swi -- switch


type alias Net =
    { root : Tree
    , redexes : List Redex
    }


type alias Program =
    { net : Net
    , book : Book
    }


type alias Book =
    Dict String Net


type alias Redex =
    -- interaction (triangles facing one an other)
    ( Tree, Tree )


reduce : WireGenerator -> Program -> Program
reduce wg ({ net, book } as prog) =
    let
        reduceNet wg_ newNet =
            { prog | net = newNet }
                |> reduce wg_
    in
    case net.redexes of
        ( Wire w, subTree ) :: queue ->
            -- LINK
            let
                link_ =
                    link subTree w
            in
            { root = link_ net.root
            , redexes = List.map (Tuple.mapBoth link_ link_) queue
            }
                |> reduceNet wg

        ( Nullary _, Nullary _ ) :: queue ->
            -- VOID
            { net | redexes = queue }
                |> reduceNet wg

        ( Nullary (Num n), Binary Ope (Nullary (Num m)) a ) :: queue ->
            -- OPERATE 1
            let
                newNum =
                    case ( n, m ) of
                        ( Number i, UnApp f ) ->
                            PartApp (f i)

                        ( UnApp f, Number i ) ->
                            PartApp (f i)

                        ( Number i, PartApp f ) ->
                            Number (f i)

                        ( PartApp f, Number i ) ->
                            Number (f i)

                        _ ->
                            ("Can't make " ++ Debug.toString n ++ " and " ++ Debug.toString m ++ " interact.")
                                |> Debug.todo
            in
            { net | redexes = ( Nullary (Num newNum), a ) :: queue }
                |> reduceNet wg

        ( (Nullary (Num _)) as n, Binary Ope a b ) :: queue ->
            -- OPERATE 2
            { net | redexes = ( a, Binary Ope n b ) :: queue }
                |> reduceNet wg

        ( (Nullary (Ref _)) as a, Binary Dup l r ) :: queue ->
            -- ERASE
            { net | redexes = ( a, l ) :: ( a, r ) :: queue }
                |> reduceNet wg

        ( Nullary (Ref ref), b ) :: queue ->
            -- CALL
            case Dict.get ref book of
                Just a ->
                    { net | redexes = ( a.root, b ) :: a.redexes ++ queue }
                        |> reduceNet wg

                Nothing ->
                    ("Missing definition : " ++ ref)
                        |> Debug.todo

        ( (Nullary _) as a, Binary _ l r ) :: queue ->
            -- ERASE
            { net | redexes = ( a, l ) :: ( a, r ) :: queue }
                |> reduceNet wg

        ( Binary a la ra, Binary b lb rb ) :: queue ->
            if a == b then
                -- ANNIHILATE
                { net | redexes = ( la, lb ) :: ( ra, rb ) :: queue }
                    |> reduceNet wg

            else if (a == Con && b == Swi) || (a == Swi && b == Con) then
                -- The paper is unclear on this
                Debug.todo "Con-Swi interaction creates undefined behavior."

            else
                -- COMMUTE
                let
                    ( x, y, ( z, w, newWG ) ) =
                        gen2Wires wg |> Tuple3.mapThird gen2Wires
                in
                { net
                    | redexes =
                        ( Binary a x y, la )
                            :: ( Binary a z w, ra )
                            :: ( Binary b x z, lb )
                            :: ( Binary b y w, rb )
                            :: queue
                }
                    |> reduceNet newWG

        ( a, b ) :: queue ->
            -- SYMMETRY
            { net | redexes = ( b, a ) :: queue }
                |> reduceNet wg

        _ ->
            prog


link : Tree -> WRef -> Tree -> Tree
link insert atWireRef into =
    case into of
        Wire w ->
            if w == atWireRef then
                insert

            else
                into

        Binary node a b ->
            Binary node
                (link insert atWireRef a)
                (link insert atWireRef b)

        _ ->
            into


gen2Wires : WireGenerator -> ( Tree, Tree, WireGenerator )
gen2Wires wg =
    ( Wire (WRef wg.highest), Wire (WRef (wg.highest + 1)), { wg | highest = wg.highest + 2 } )


{-| Net builder serves an a neat api to build and compose valid nets without dealing with wire reference uniqueness.
-}
type BNet
    = N NetGen
    | W String
      -- `C` stands for combine
    | C1 BNet NetGen1
    | C2 BNet BNet NetGen2
    | C3 BNet BNet BNet NetGen3


type alias NetGen =
    WireGenerator -> ( Net, WireGenerator )


type alias NetGen1 =
    WireGenerator -> Net -> ( Net, WireGenerator )


type alias NetGen2 =
    WireGenerator -> Net -> Net -> ( Net, WireGenerator )


type alias NetGen3 =
    WireGenerator -> Net -> Net -> Net -> ( Net, WireGenerator )


{-| Used to ensure wire references are unique
-}
type alias WireGenerator =
    { highest : Int
    , mapping : Dict String Int
    }


type WRef
    = WRef Int
