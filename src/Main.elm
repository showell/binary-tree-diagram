module Main exposing (main)

import BinaryTreeDiagram
    exposing
        ( BinaryTree(..)
        , diagramView
        )
import Browser
import Html
import Html.Attributes exposing (style)
import Random



-- MODEL / INIT


type alias Model =
    { title : String
    }


type alias Msg =
    Never


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { title = "binary tree examples"
            }
    in
    ( model, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


makeTree num balanced =
    let
        startSeed =
            Random.initialSeed 42

        avg lo hi =
            (lo + hi) // 2

        makeNode seed n =
            let
                ( nn, newSeed ) =
                    Random.step (Random.int 1 4) seed

                color =
                    case nn of
                        1 ->
                            "blue"

                        2 ->
                            "red"

                        3 ->
                            "green"

                        _ ->
                            "purple"

                v =
                    { text = String.fromInt n
                    , color = color
                    }
            in
            ( v, newSeed )

        f seed lo hi =
            if lo > hi then
                ( Empty, seed )

            else if lo == hi then
                let
                    ( node, seed2 ) =
                        makeNode seed lo

                    tree =
                        Node node Empty Empty
                in
                ( tree, seed2 )

            else
                let
                    ( mid, seed2 ) =
                        if balanced then
                            ( avg lo hi, seed )

                        else
                            Random.step (Random.int lo hi) seed

                    ( left, seed3 ) =
                        f seed2 lo (mid - 1)

                    ( right, seed4 ) =
                        f seed3 (mid + 1) hi

                    ( node, seed5 ) =
                        makeNode seed4 mid

                    tree =
                        Node node left right
                in
                ( tree, seed5 )
    in
    f startSeed 1 num
        |> Tuple.first


about =
    """
    Welcome to the demo!

    This is a demo of the showell/binary-tree-diagram package.

    All it really does is draw binary trees.

    The colors here are just randomly chosen.
    """
        |> Html.text
        |> List.singleton
        |> Html.p []


view : Model -> Browser.Document Msg
view model =
    let
        diagram tree =
            diagramView (\v -> v.color) (\v -> v.text) tree
                |> List.singleton
                |> Html.div [ style "padding" "50px" ]

        example n balanced =
            let
                bal =
                    if balanced then
                        "(balanced)"

                    else
                        "(unbalanced)"

                desc =
                    String.fromInt n
                        ++ " nodes "
                        ++ bal
            in
            [ Html.h3 [] [ Html.text desc ]
            , diagram (makeTree n balanced)
            ]
                |> Html.div []

        body =
            [ about
            , example 15 True
            , example 20 True
            , example 31 True
            , example 63 True
            , example 127 True
            , example 4095 True
            , example 50 False
            , example 150 False
            , example 500 False
            ]
                |> Html.div [ style "width" "750px", style "padding" "50px" ]
                |> List.singleton
    in
    { title = model.title
    , body = body
    }
