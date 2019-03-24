module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, text, div, h1, img, p, pre)
import Html.Attributes exposing (class, classList, id, src, style, tabindex)
import Html.Events exposing (on)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Task

type alias Flags = {}

---- MODEL ----

type ActiveView =
    Welcome | Summary | Experience | Education | Contact | Links | Feedback | Language | Theme

type alias Model =
    {
        activeView: ActiveView
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {
        activeView = Welcome
    }, Dom.focus "outermost" |> Task.attempt (always NoOp) )



---- UPDATE ----

type Msg
    = HandleKeyboardEvent KeyboardEvent
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent event ->
            let
                newView = getActiveView event.ctrlKey event.key
            in
            case newView of
                Just v ->
                    ( { model | activeView = v }
                    , Cmd.none
                    )
                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

getActiveView : Bool -> Maybe String -> Maybe ActiveView
getActiveView ctrl event =
    if (not ctrl) then Nothing
    else
        case event of
            Just key ->
                case key of
                    "s" ->
                        Just Summary
                    "w" ->
                        Just Experience
                    "c" ->
                        Just Contact
                    "e" ->
                        Just Education
                    "f" ->
                        Just Feedback
                    "z" ->
                        Just Language
                    "t" ->
                        Just Theme
                    "l" ->
                        Just Links
                    _ ->
                        Nothing
            Nothing ->
                Nothing


---- VIEW ----


view : Model -> Html Msg
view model =
    div [ on "keydown" <|
            Json.map HandleKeyboardEvent decodeKeyboardEvent
        , tabindex 0
        , id "outermost"
        , style "position" "absolute"
        , style "height" "100%"
        , style "width" "100%"
        , style "overflow" "hidden"
        , style "outline" "none"
        ]
        [
            div [class "h-screen w-screen"]
                [ topBar, body model.activeView
                ]
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

body activeView =
    div [class "terminal bg-black"] [terminalHeader, div [class "terminal-content"] [terminalContent activeView], terminalFooter]

terminalContent activeView =
    case activeView of
        Welcome -> 
            text "Welcome"
        Summary ->
            text "Summary"
        _ -> 
            text "Section coming soon"
  

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
    div [class "flex flex-row text-white p-1 w-1/6"] [
        p [class "m-2 bg-grey-light text-black"] [text key],
        p [class "m-2"] [text description]
    ]

---- SUBSCRIPTIONS ----

subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown (Json.map HandleKeyboardEvent decodeKeyboardEvent)

---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
