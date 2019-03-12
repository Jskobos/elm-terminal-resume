module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class, classList, src)


---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [class "h-screen w-screen"]
        [ topBar, body
        ]

topBar =
    div [class "flex flex-row items-center justify-start topbar bg-grey-darkest"] [
        dot "bg-red-light",
        dot "bg-yellow",
        dot "bg-green"
    ]

dot : String -> Html Msg
dot color = 
    div [classList
        [ (color, True)
        , ("dot", True),
          ("m-1", True)
        ] ] []

body =
    div [class "terminal bg-black"] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
