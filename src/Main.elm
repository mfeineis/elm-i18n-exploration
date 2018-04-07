port module Main exposing (main)

import Dict exposing (Dict)
import History exposing (History)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onCheck, onClick)
import Intl exposing (TranslationKey, TranslationMode(..), TranslationValue)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


port storeTranslations : Value -> Cmd msg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- TODO: we need to filter `Sub`s when in `Editing` mode!
    Sub.batch
        []


type alias Model =
    { focusedTranslatable : TranslationKey
    , focusedValue : TranslationValue
    , history : History AppModel
    , historyMode : HistoryMode
    , i18nLookup : Dict TranslationKey TranslationValue
    , translationMode : TranslationMode
    }


type Msg
    = AppMsg AppMsg
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


type AppMsg
    = Increment


type alias AppModel =
    { counter : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { focusedTranslatable = ""
      , focusedValue = ""
      , history = History.init { counter = 0 }
      , historyMode = Interactive
      , i18nLookup = Dict.empty
      , translationMode = NotEditing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ history, historyMode, translationMode } as model) =
    case msg of
        AppMsg appMsg ->
            if historyMode == Interactive && translationMode == NotEditing then
                let
                    ( appModel, cmd ) =
                        History.present history
                            |> updateApp appMsg
                in
                if appModel == History.present history then
                    ( model, cmd )
                else
                    ( { model
                        | history = History.append appModel model.history
                      }
                    , cmd
                    )
            else
                model |> Debug.log ("Not interactive! Dropping " ++ toString appMsg) |> withoutCmd

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


updateApp : AppMsg -> AppModel -> ( AppModel, Cmd Msg )
updateApp msg ({ counter } as model) =
    case msg of
        Increment ->
            { model | counter = counter + 1 }
                |> withoutCmd


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


view : Model -> Html Msg
view ({ history, i18nLookup, translationMode } as model) =
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
        (List.concat
            [ [ toolbar model ]
            , renderApp model (History.present history)
            ]
        )


renderApp : Model -> AppModel -> List (Html Msg)
renderApp ctx { counter } =
    [ someLabel ctx []
    , someButton ctx
        [ onClick (AppMsg Increment)
        ]
    , Html.text (toString counter)
    , someLabel ctx [ Attr.class "--different" ]
    ]


someButton : Model -> List (Attribute Msg) -> Html Msg
someButton =
    i15d Html.button "Some Button" "some.button"


someLabel : Model -> List (Attribute Msg) -> Html Msg
someLabel =
    i15d Html.div "Hello, World!" "some.label"


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
