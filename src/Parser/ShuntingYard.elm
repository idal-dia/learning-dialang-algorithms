module Parser.ShuntingYard exposing (..)

{-| This first parser uses the [shunting yard algorithm](https://en.wikipedia.org/wiki/Shunting_yard_algorithm) to parse binary operators with various precedences.
-}

import Dict exposing (Dict)
import Lexer.IntMath as L
import Parser as P exposing (Parser)


type Tree
    = Op String Tree Tree
    | Int Int


type alias OpTable =
    { prec : Dict String Int -- precedence of each operator
    , assoc : Dict Int Assoc -- associativity for each precedence
    }


type Assoc
    = Left
    | Right


parser : OpTable -> Parser Tree
parser table =
    P.map (shuntingYard table) L.parser


shuntingYard : OpTable -> List L.Token -> Tree
shuntingYard table tokens =
    let
        rec : List L.Token -> List String -> List Tree -> Tree
        rec tokens_ opStack acc =
            case ( tokens_, opStack ) of
                ( [], _ ) ->
                    case apply opStack acc of
                        [ tree ] ->
                            tree

                        _ ->
                            Debug.todo "The tree is empty or disconnected."

                ( (L.Int i) :: queue, _ ) ->
                    rec queue opStack (Int i :: acc)

                ( (L.Op op) :: queue, [] ) ->
                    rec queue [ op ] acc

                ( (L.Op opA) :: queue, opB :: _ ) ->
                    case ( Dict.get opA table.prec, Dict.get opB table.prec ) of
                        ( Nothing, _ ) ->
                            Debug.todo ("Unknown operator : " ++ opA)

                        ( _, Nothing ) ->
                            Debug.todo ("Unknown operator : " ++ opB)

                        ( Just opAPrec, Just opBPrec ) ->
                            if
                                (opAPrec > opBPrec)
                                    || (opAPrec == opBPrec && Dict.get opAPrec table.assoc == Just Right)
                            then
                                rec queue (opA :: opStack) acc

                            else
                                rec queue [ opA ] (apply opStack acc)

        apply : List String -> List Tree -> List Tree
        apply opStack acc =
            case ( opStack, acc ) of
                ( [], _ ) ->
                    acc

                ( op :: opStackQueue, b :: a :: accQueue ) ->
                    (Op op a b :: accQueue)
                        |> apply opStackQueue

                ( op :: _, _ ) ->
                    Debug.todo ("Less than two operands for the " ++ op ++ " binary operator.")
    in
    rec tokens [] []
