module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, a, text, div, h1, img, input, p, pre, span, textarea)
import Html.Attributes exposing (autofocus, class, classList, cols, href, id, placeholder, rows, src, style, tabindex, value)
import Html.Events exposing (on, onInput)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Task

type alias Flags = {}

---- MODEL ----

type ActiveView =
    Welcome | Summary | Experience | Education | Links | Feedback | Language | Theme

type alias Model =
    {
        activeView: ActiveView,
        activeTheme: ThemeOption,
        inputText: String
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {
        activeView = Welcome,
        activeTheme = Classic,
        inputText = ""
    }, Dom.focus "outermost" |> Task.attempt (always NoOp) )



---- UPDATE ----

type KeyAction =
    ChangeTheme
    | ChangeView

type Msg
    = HandleKeyboardEvent KeyAction KeyboardEvent
    | HandleViewChange KeyboardEvent
    | HandleThemeInput KeyboardEvent
    | ThemeChange ThemeOption
    | TextInput String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent action event ->
            case action of
                ChangeTheme ->
                    update (HandleThemeInput event) model
                ChangeView ->
                    update (HandleViewChange event) model

        HandleViewChange event ->
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
        TextInput input ->
            ( { model | inputText = input }, Cmd.none )
        ThemeChange newTheme ->
            ( { model | activeTheme = newTheme }, Cmd.none )
        HandleThemeInput event ->
            let
                newTheme = getNewTheme event.ctrlKey event.key
            in
            case newTheme of
                Just t ->
                    ( { model | activeTheme = t }, Cmd.none)
                Nothing ->
                    update (HandleViewChange event) model
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

getNewTheme : Bool -> Maybe String -> Maybe ThemeOption
getNewTheme ctrl event =
    if (ctrl) then Nothing
    else
        case event of
            Just key ->
                case key of
                    "1" ->
                        Just Classic
                    "2" ->
                        Just Green
                    _ ->
                        Nothing
            Nothing ->
                Nothing


---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [
            div [class "h-screen w-screen"]
                [ topBar, body model
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

body model =
    div [class "terminal bg-black"] [terminalHeader model.activeView model.activeTheme, div [class "terminal-content"] [terminalContent model], terminalFooter model.activeView model.activeTheme]

terminalContent model =
    let
        themeClasses = case model.activeTheme of
            Classic ->
                " text-white"
            Green ->
                " green-theme-text"
    in
    
    div [class themeClasses] [
        case model.activeView of
                Welcome -> 
                    welcome model
                Summary ->
                    summary
                Education ->
                    education
                Experience ->
                    experience
                Links ->
                    links
                Theme ->
                    theme model.activeTheme
                _ -> 
                    text "Section coming soon"
    ]
    
  


terminalFooter : ActiveView -> ThemeOption -> Html Msg
terminalFooter terminalView currentTheme =
    let
        footer = footerItem currentTheme
    in    
    div [class "terminal-footer"] [
        div [class "flex flex-row flex-wrap"] [
            footer "^S" "Summary",
            footer "^W" "Work Experience",
            footer "^E" "Education",
            footer "^L" "Links",
            footer "^F" "Leave feedback",
            footer "^Z" "Change Language",
            footer "^T" "Change Theme"
        ]]

terminalHeader activeView activeTheme =
    let
        themeClasses = case activeTheme of
            Classic ->
                " bg-grey-light"
            Green ->
                " green-background"
    in
    div [class ("terminal-header")] [
        div [class ("w-full flex flex-row items-center justify-start p-1" ++ themeClasses)] [
            p [class "w-1/3 flex justify-start"] [text "JSK resume 0.0.1"],
            p [class "w-2/3 flex justify-start"] [text (if activeView /= Welcome then "File: " ++ (headerText activeView) else headerText activeView)]
        ]
    ]

headerText : ActiveView  -> String
headerText currentView =
    case currentView of
        Summary ->
            "summary.txt"
        Education ->
            "education.txt"
        Experience ->
            "work_experience.txt"
        Feedback ->
            "feedback_form.txt"
        Theme ->
            "theme.txt"
        Language ->
            "language_select.txt"
        Welcome ->
            "New Buffer"
        Links ->
            "links.txt"


footerItem : ThemeOption -> String -> String -> Html Msg
footerItem currentTheme key description =
    let
        keyClasses = case currentTheme of
            Classic ->
                "m-2 bg-grey-light text-black"
            Green ->
                "m-2 green-background text-black"
        descClasses = case currentTheme of
            Classic ->
                "m-2 text-white"
            Green ->
                "m-2 green-theme-text"
    in
    
    div [class "flex flex-row p-1 w-1/6"] [
        p [class keyClasses] [text key],
        p [class descClasses] [text description]
    ]


--- DISPLAY SECTIONS ---


sectionTitle : String -> Html Msg
sectionTitle title =
    p [class "text-2xl"] [text title]

summary =
    div [class "text-left ml-2 body-text"] [
        p [] [text "Jared Kobos"],
        p [] [text "JavaScript Developer at Linode"],
        p [] [text "Build things with React, Redux, Jest, Typescript, and Hugo. Also a fan of Elm, Go, and Python."]
    ]

education =
    div [class "text-left ml-2 body-text flex flex-col justify-between h-full"] [
        div [] [
            sectionTitle "Education",
            p [] [text "Bachelor of Music Education (University of Delaware)"],
            p [] [text "Master of Music (Florida State University)"],
            p [] [text "Doctor of Musical Arts* (Michigan State University)"]
        ],
        div [] [
            p [class "text-right"] [text "* (it's a long story)"]
        ]     
    ]

type alias WorkItem = 
    {
        company : String,
        position : String,
        location : String,
        start : String,
        end : String,
        description : List String
    }

items = 
    [
        WorkItem "Linode" "JavaScript Developer" "Philadelphia, PA" "05-01-18" "" [
            "Build and maintain features for front end applications",
            "Coordinate releases",
            "Research and present patterns to improve the codebase"
            ],
        WorkItem "Linode" "Technical Writer" "Philadelphia, PA" "09-06-17" "05-01-18" [
            "Write, edit, and tech edit documentation on Linux-related topics",
            "Use continuous integration and scripting to improve quality of documentation library"
            ],
        WorkItem "EF Education First" "Content Writer" "Shanghai, CN" "02-04-16" "08-21-17" [
            "Write textbook and online content for an international education company"
            ],
        WorkItem "EF Education First" "International Teacher" "Shanghai, CN" "07-18-13" "02-04-16" [
            "Teach English to Chinese kids"
            ]
    ]

experience =
    div [class "text-left ml-2 body-text"] [
        sectionTitle "Work Experience",
        div [] (List.map renderWorkItem items)
    ]

renderWorkItem : WorkItem -> Html Msg
renderWorkItem item =
    let 
        dates = if item.end == "" then (item.start ++ " - Present")
                else (item.start ++ " - " ++ item.end)
    in 
    div [class "mt-6"] [
        p [class "leading-tight w-full flex flex-row justify-between"] [
            span [] [text (item.position ++ "  :  " ++ (item.company ++ " (" ++ item.location ++ ")"))],
            span [class "mr-6"] [text dates]
        ],
        div [class "leading-normal"] (List.map renderDescription item.description)
    ]

renderDescription : String -> (Html Msg)
renderDescription desc =
    p [] [text (" - " ++ desc)]

links =
    div [class "text-left ml-2 body-text"] [
        sectionTitle "Links",
        renderLinkItem "https://github.com/jskobos" "GitHub Profile",
        renderLinkItem "https://gitbhub.com/jskobos/old-resume" "Previous GitHub portfolio (Vanilla Javascript MVC)"
    ]

renderLinkItem url description =
    p [class "mt-6 ml-4"] [a [href url, class "link-item"] [text description]]

welcome model =
    div [class "flex flex-column justify-start w-full"] [
        text ""
    ]

feedback model =
    div [class "flex flex-column justify-start w-full h-full"] [
        textarea [autofocus True, cols 40, rows 20, placeholder "Leave some feedback..." ,value model.inputText, onInput TextInput, class "bg-black text-white text-left w-full" ] []
    ]

type ThemeOption = Classic | Green

theme : ThemeOption -> Html Msg
theme activeTheme =
    div [class "text-left ml-2 body-text"] [
        sectionTitle "Choose a Theme"
        , renderOptions activeTheme
    ]

renderOptions activeTheme =
    div [class "m-2"] [
        div [class "p-2 flex flex-row justify-start"] [
                div [class "mr-2"] [text "1: "],
                div [] [text "Normal"]
            ],

        div [class "p-2 flex flex-row justify-start"] [
            div [class "mr-2"] [text "2: "],
            div [] [text "Old-School Green"]
        ]
    ]
    


---- SUBSCRIPTIONS ----

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        mode = getMode model.activeView
    in 
    onKeyDown (Json.map (HandleKeyboardEvent mode) decodeKeyboardEvent)

getMode v =
    case v of
        Theme ->
            ChangeTheme
        _ ->
            ChangeView

---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
