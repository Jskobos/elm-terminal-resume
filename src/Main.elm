module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, p, pre)
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
        div [class "flex flex-row items-center justify-center pl-2"] [dot "bg-red-light",
        dot "bg-yellow",
        dot "bg-green"]
    ]

dot : String -> Html Msg
dot color = 
    div [classList
        [ (color, True)
        , ("dot", True),
          ("m-1", True)
        ] ] []

body =
    div [class "terminal bg-black"] [terminalHeader, terminalContent "", terminalFooter]

terminalContent content =
    div [class "terminal-content"] [text content]

terminalFooter =
    div [class "terminal-footer"] [
        div [class "flex flex-row"] [
            footerItem "^S" "Summary",
            footerItem "^W" "Work Experience",
            footerItem "^E" "Education",
            footerItem "^C" "Contact Info",
            footerItem "^L" "Links",
            footerItem "^F" "Leave feedback",
            footerItem "^Z" "Change Language",
            footerItem "^T" "Change Theme"
        ]
    ]

terminalHeader =
    div [class "terminal-header "] [
        div [class "bg-grey-light w-full flex flex-row items-center justify-start p-1"] [
            p [class "w-1/3 flex justify-start"] [text "JSK resume 0.0.1"],
            p [class "w-2/3 flex justify-start"] [text "New Buffer"]
        ]
    ]

footerItem : String -> String -> Html Msg
footerItem key description =
    div [class "flex flex-row text-white p-1 mr-32"] [
        p [class "m-2 bg-grey-light text-black"] [text key],
        p [class "m-2"] [text description]
    ]

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
