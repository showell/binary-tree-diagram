module Main exposing (main)

import BinaryTreeDiagram
    exposing
        ( BinaryTree(..)
        , diagramView
        )
import Browser
import Html



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


view : Model -> Browser.Document Msg
view model =
    let
        simpleTree =
            Node { color = "red", text = "2" }
                (Node { color = "blue", text = "1" } Empty Empty)
                (Node { color = "green", text = "3" } Empty Empty)

        diagram =
            diagramView (\v -> v.color) (\v -> v.text) simpleTree

        body =
            [ diagram
            ]
    in
    { title = model.title
    , body = body
    }
