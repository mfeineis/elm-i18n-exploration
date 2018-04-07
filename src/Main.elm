port module Main exposing (main)

import App
import Dict exposing (Dict)
import History exposing (History)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onCheck, onClick)
import Intl exposing (TranslationKey, TranslationMode(..), TranslationValue)
import Json.Decode as Decode exposing (Decoder, Value)
import Vigor


port storeTranslations : Value -> Cmd msg


main : Program Value Model Msg
main =
    let
        counter =
            App.vigor
                { incoming =
                    \msg { historyMode, translationMode } ->
                        case msg of
                            AppMsg it ->
                                if historyMode == Interactive && translationMode == NotEditing then
                                    Just it
                                else
                                    Nothing |> Debug.log ("Not interactive! Dropping " ++ toString it)

                            _ ->
                                Nothing
                , outgoing = AppMsg
                , read =
                    \{ history } ->
                        History.present history
                , store =
                    \({ history } as model) appModel ->
                        if appModel == History.present history then
                            model
                        else
                            { model | history = History.append appModel history }
                }

        intl =
            Intl.vigor
                { incoming =
                    \msg _ ->
                        case msg of
                            IntlMsg it ->
                                Just it

                            _ ->
                                Nothing
                , outgoing = IntlMsg
                , read = \{ intl } -> intl
                , store = \model intl -> { model | intl = intl }
                }

        compositeView model =
            view { counter = counter.view model } model

        shell =
            { init = init
            , subscriptions = \_ -> Sub.none
            , update = update
            , view = compositeView
            }
    in
    Html.programWithFlags <|
        Vigor.compose shell
            [ counter
            , intl
            ]


type alias Model =
    { history : History App.Model
    , historyMode : HistoryMode
    , intl : Intl.Context
    }


type Msg
    = AppMsg App.Intent
    | HistoryMsg HistoryMsg
    | IntlMsg Intl.Msg
    | ToggleHistoryMode Bool


type HistoryMsg
    = StepBack
    | StepForward


type HistoryMode
    = Interactive
    | ReadOnly


type alias Flags =
    { translations : Dict TranslationKey TranslationValue
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map Flags
        (Decode.field "translations" (Decode.dict Decode.string))


init : Value -> ( Model, Cmd Msg )
init json =
    let
        flags =
            case Decode.decodeValue flagsDecoder json of
                Ok it ->
                    Debug.log "Flags: " it

                Err reason ->
                    Debug.log ("Flags invalid: " ++ reason)
                        { translations = Dict.empty
                        }
    in
    ( { history = History.init (App.init 0)
      , historyMode = Interactive
      , intl = Intl.init flags.translations
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ history, historyMode, translationMode } as model) =
    case msg of
        AppMsg _ ->
            model |> withoutCmd

        HistoryMsg historyMsg ->
            model |> handleHistoryMsg historyMsg

        IntlMsg _ ->
            model |> withCmds [ storeTranslations (encodeLookup i18nLookup) ]

        ToggleHistoryMode shouldBeInteractive ->
            let
                toggle model =
                    { model
                        | historyMode =
                            if shouldBeInteractive then
                                Interactive
                            else
                                ReadOnly
                    }
            in
            model |> toggle |> withoutCmd


handleHistoryMsg : HistoryMsg -> Model -> ( Model, Cmd Msg )
handleHistoryMsg msg ({ history } as model) =
    case msg of
        StepBack ->
            { model | history = History.stepBack history }
                |> Debug.log "Stepped backward."
                |> withoutCmd

        StepForward ->
            { model | history = History.stepForward history }
                |> Debug.log "Stepped forward."
                |> withoutCmd



-- View


type alias Partials =
    { counter : Html Msg
    }


view : Partials -> Model -> Html Msg
view partials ({ history, i18nLookup, translationMode } as model) =
    let
        editing =
            translationMode == Editing
    in
    Html.div
        [ if editing then
            Attr.class "i18n--editing"
          else
            Attr.class ""
        ]
        [ toolbar model
        , partials.counter
        ]


toolbar : Model -> Html Msg
toolbar model =
    Html.div []
        (List.concat
            [ [ baseStyle ]
            , toggleModeButton model
            , historyToolbar model
            , [ Html.hr [] [] ]
            ]
        )


toggleModeButton : Model -> List (Html Msg)
toggleModeButton model =
    [ Html.button
        [ onClick ToggleTranslationMode
        ]
        [ Html.text "Switch"
        ]
    ]


historyToolbar : Model -> List (Html Msg)
historyToolbar ({ history } as model) =
    [ Html.button
        [ Attr.disabled (not (History.canStepBack history))
        , onClick (HistoryMsg StepBack)
        ]
        [ Html.text "<-"
        ]
    , Html.button
        [ Attr.disabled (not (History.canStepForward history))
        , onClick (HistoryMsg StepForward)
        ]
        [ Html.text "->"
        ]
    ]
        ++ historyModeToggle model


historyModeToggle : { a | historyMode : HistoryMode } -> List (Html Msg)
historyModeToggle { historyMode } =
    [ Html.label []
        [ Html.input
            [ Attr.checked (historyMode == Interactive)
            , onCheck ToggleHistoryMode
            , Attr.type_ "checkbox"
            ]
            []
        , Html.text "Interactive?"
        ]
    ]



-- Helpers


withCmds : List (Cmd msg) -> model -> ( model, Cmd msg )
withCmds cmds model =
    ( model, Cmd.batch cmds )


withoutCmd : model -> ( model, Cmd msg )
withoutCmd model =
    ( model, Cmd.none )


baseStyle : Html msg
baseStyle =
    Html.node "style"
        []
        [ Html.text
            """

html {
    box-sizing: border-box;
}

body {
    margin: 0;
    padding: 0;
}

*, *:before, *:after {
    box-sizing: inherit;
}

.i18n--editing [data-i18n] {
    border: 3px dotted red;
}

.i18n--editing [data-i18n].focused {
    background-color: #fdd;
}

pre {
    background: #eee;
    border: 1px solid #ddd;
}

            """
        ]
