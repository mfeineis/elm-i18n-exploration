port module Main exposing (main)

import Data.Translation as Translation
import Env
import History exposing (History)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onCheck, onClick)
import Http
import Intl exposing (TranslationKey, TranslationMode(..), TranslationValue)
import Json.Decode as Decode exposing (Decoder, Value)


port storeTranslations : Value -> Cmd msg


main : Program Value Model Msg
main =
    Html.programWithFlags
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
    , i18n : Intl.Lookup
    , translationMode : TranslationMode
    }


type Msg
    = AppMsg AppMsg
    | FocusTranslatable TranslationKey TranslationValue
    | HistoryMsg HistoryMsg
    | ToggleHistoryMode Bool
    | ToggleTranslationMode
    | TranslationRequested (Result Http.Error Intl.Lookup)
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


type alias Flags =
    { locales : List Intl.Locale
    , translations : Intl.Lookup
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map2 Flags
        (Decode.field "locales" (Decode.list Intl.localeDecoder))
        (Decode.field "translations" Translation.decoder)


init : Value -> ( Model, Cmd Msg )
init json =
    let
        flags =
            case Decode.decodeValue flagsDecoder json of
                Ok it ->
                    it

                Err reason ->
                    Debug.log ("Flags invalid: " ++ reason)
                        { locales = [ Intl.locale "en-US" ]
                        , translations = Intl.empty
                        }

        _ =
            Debug.log "flags: " flags

        _ =
            Debug.log "env: " Env.id
    in
    ( { focusedTranslatable = ""
      , focusedValue = ""
      , history = History.init { counter = 0 }
      , historyMode = Interactive
      , i18n = flags.translations
      , translationMode = NotEditing
      }
    , Http.send TranslationRequested (Translation.request flags.locales)
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

        TranslationRequested (Ok lookup) ->
            model
                |> (\model -> { model | i18n = lookup })
                |> Debug.log "Fetched translations"
                |> withoutCmd

        TranslationRequested (Err reason) ->
            model |> Debug.log ("Failed to fetch translations: " ++ toString reason) |> withoutCmd

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
toggleTranslationMode ({ i18n, translationMode } as model) =
    case translationMode of
        Editing ->
            Debug.log "Done editing..."
                { model | translationMode = NotEditing }
                |> withCmds
                    [ storeTranslations (Intl.encode i18n)
                    ]

        NotEditing ->
            { model | translationMode = Editing }
                |> withoutCmd


updateTranslation : TranslationKey -> TranslationValue -> Model -> ( Model, Cmd Msg )
updateTranslation key value ({ i18n } as model) =
    -- TODO: sanitize user input!
    { model
        | i18n = Intl.insert key value i18n
    }
        |> Debug.log ("Sanitized: " ++ value)
        |> withoutCmd



-- View


view : Model -> Html Msg
view ({ history, i18n, translationMode } as model) =
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
    , searchInput ctx []
    ]


someButton : Model -> List (Attribute Msg) -> Html Msg
someButton =
    i15d Html.button "Some Button" "some.button"


searchInput : Model -> List (Attribute Msg) -> Html Msg
searchInput =
    i15dWithPlaceholder Html.input "Search here..." "some.search"


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
            , languageSwitch model
            , [ Html.hr [] [] ]
            ]
        )


languageSwitch : Model -> List (Html Msg)
languageSwitch { i18n } =
    let
        option locale =
            Html.option
                [ Attr.value locale ]
                [ Html.text locale ]
    in
    [ Html.select
        [--Events.on "change" selectChangeDecoder SelectLanguage
        ]
        (List.map option (Intl.supportedLocales i18n))
    ]


selectChangeDecoder : Decoder Int
selectChangeDecoder =
    Decode.at [ "target", "valueAsNumber" ] Decode.int


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
i15d element defaultValue key ({ focusedTranslatable, focusedValue, i18n, translationMode } as model) attrs =
    -- TODO: support for multi-line values?
    let
        ( value, focusAttrs ) =
            if focusedTranslatable == key then
                ( focusedValue, [ Attr.class "focused" ] )
            else
                ( Intl.get defaultValue key i18n, [ Attr.class "" ] )
    in
    element (i18nAttr key value model ++ focusAttrs ++ attrs)
        [ if focusedTranslatable == key && translationMode == Editing then
            -- While the element is focused we don't want Elm to tinker with the node
            Html.text focusedValue
          else
            Html.text (Intl.get defaultValue key i18n)
        ]


i15dWithPlaceholder : (List (Attribute Msg) -> List (Html Msg) -> Html Msg) -> String -> TranslationKey -> Model -> List (Attribute Msg) -> Html Msg
i15dWithPlaceholder element defaultValue key ({ focusedTranslatable, focusedValue, i18n, translationMode } as model) attrs =
    -- TODO: support for multi-line values?
    let
        ( value, focusAttrs ) =
            if focusedTranslatable == key then
                ( focusedValue, [ Attr.class "focused" ] )
            else
                ( Intl.get defaultValue key i18n, [ Attr.class "" ] )

        placeholder =
            [ if focusedTranslatable == key && translationMode == Editing then
                -- While the element is focused we don't want Elm to tinker with the node
                Attr.placeholder focusedValue
              else
                Attr.placeholder (Intl.get defaultValue key i18n)
            ]
    in
    element (i18nAttr key value model ++ focusAttrs ++ attrs ++ placeholder) []


i18nAttr : TranslationKey -> TranslationValue -> { a | translationMode : TranslationMode } -> List (Attribute Msg)
i18nAttr key value { translationMode } =
    Intl.i18n key (FocusTranslatable "" "") (FocusTranslatable key value) (UpdateTranslation key) translationMode


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

input[contenteditable=true] {
  /*content: attr(placeholder);*/
  display: block; /* For Firefox */
}

pre {
    background: #eee;
    border: 1px solid #ddd;
}

            """
        ]
