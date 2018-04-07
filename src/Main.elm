port module Main exposing (main)

import App
import Dict exposing (Dict)
import History exposing (History)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onCheck, onClick)
import Intl exposing (TranslationKey, TranslationMode(..), TranslationValue)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
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
            ]


type alias Model =
    { focusedTranslatable : TranslationKey
    , focusedValue : TranslationValue
    , history : History App.Model
    , historyMode : HistoryMode
    , i18nLookup : Dict TranslationKey TranslationValue
    , translationMode : TranslationMode
    }


type Msg
    = AppMsg App.Intent
    | FocusTranslatable TranslationKey TranslationValue
    | HistoryMsg HistoryMsg
    | ToggleHistoryMode Bool
    | ToggleTranslationMode
    | UpdateTranslation TranslationKey TranslationValue


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
    ( { focusedTranslatable = ""
      , focusedValue = ""
      , history = History.init (App.init 0)
      , historyMode = Interactive
      , i18nLookup = flags.translations
      , translationMode = NotEditing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ history, historyMode, translationMode } as model) =
    case msg of
        AppMsg _ ->
            model |> withoutCmd

        FocusTranslatable key value ->
            let
                focus model =
                    { model
                        | focusedTranslatable = key
                        , focusedValue = value
                    }
            in
            model |> focus |> withoutCmd

        HistoryMsg historyMsg ->
            model |> handleHistoryMsg historyMsg

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

        ToggleTranslationMode ->
            model |> toggleTranslationMode

        UpdateTranslation key value ->
            model |> updateTranslation key value


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


toggleTranslationMode : Model -> ( Model, Cmd Msg )
toggleTranslationMode ({ i18nLookup, translationMode } as model) =
    case translationMode of
        Editing ->
            Debug.log "Done editing..."
                { model | translationMode = NotEditing }
                |> withCmds
                    [ storeTranslations (encodeLookup i18nLookup)
                    ]

        NotEditing ->
            { model | translationMode = Editing }
                |> withoutCmd


updateTranslation : TranslationKey -> TranslationValue -> Model -> ( Model, Cmd Msg )
updateTranslation key value ({ i18nLookup } as model) =
    -- TODO: sanitize user input!
    { model
        | i18nLookup = Dict.insert key value i18nLookup
    }
        |> Debug.log ("Sanitized: " ++ value)
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


toggleModeButton : { a | translationMode : TranslationMode } -> List (Html Msg)
toggleModeButton model =
    [ Html.button
        [ onClick ToggleTranslationMode
        ]
        [ Html.text "Switch"
        ]
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


someButton : Model -> List (Attribute Msg) -> Html Msg
someButton =
    i15d Html.button "Some Button" "some.button"


someLabel : Model -> List (Attribute Msg) -> Html Msg
someLabel =
    i15d Html.div "Hello, World!" "some.label"



-- Helpers


i15d : (List (Attribute Msg) -> List (Html Msg) -> Html Msg) -> String -> TranslationKey -> Model -> List (Attribute Msg) -> Html Msg
i15d element defaultValue key ({ focusedTranslatable, focusedValue, i18nLookup, translationMode } as model) attrs =
    -- TODO: support for multi-line values?
    let
        ( value, focusAttrs ) =
            if focusedTranslatable == key then
                ( focusedValue, [ Attr.class "focused" ] )
            else
                ( Intl.lookup defaultValue key i18nLookup, [ Attr.class "" ] )
    in
    element (i18n key value model ++ focusAttrs ++ attrs)
        [ if focusedTranslatable == key && translationMode == Editing then
            -- While the element is focused we don't want Elm to tinker with the node
            Html.text focusedValue
          else
            Html.text (Intl.lookup defaultValue key i18nLookup)
        ]


i18n : TranslationKey -> TranslationValue -> { a | translationMode : TranslationMode } -> List (Attribute Msg)
i18n key value { translationMode } =
    Intl.i18n key (FocusTranslatable "" "") (FocusTranslatable key value) (UpdateTranslation key) translationMode


encodeLookup : Dict TranslationKey TranslationValue -> Value
encodeLookup =
    encodeDict identity Encode.string


encodeDict : (comparable -> String) -> (v -> Value) -> Dict comparable v -> Value
encodeDict toKey toValue dict =
    Dict.toList dict
        |> List.map (\( key, value ) -> ( toKey key, toValue value ))
        |> Encode.object


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
