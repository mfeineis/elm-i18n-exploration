module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick)
import Intl exposing (TranslationKey, TranslationMode(..), TranslationValue)
import Json.Decode as Decode exposing (Decoder, Value)


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
    Sub.batch
        []


type alias Model =
    { i18nLookup : Dict TranslationKey TranslationValue
    , translationMode : TranslationMode
    }


type Msg
    = UpdateTranslation TranslationKey TranslationValue
    | ToggleTranslationMode


init : ( Model, Cmd Msg )
init =
    ( { i18nLookup = Dict.empty, translationMode = ReadOnly }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleTranslationMode ->
            model |> toggleTranslationMode |> withoutCmd

        UpdateTranslation key value ->
            Debug.log ("UpdateTranslation " ++ key ++ ": " ++ value)
                (model |> updateTranslation key value |> withoutCmd)


toggleTranslationMode : { a | translationMode : TranslationMode } -> { a | translationMode : TranslationMode }
toggleTranslationMode ({ translationMode } as model) =
    case translationMode of
        Editing ->
            Debug.log "Done editing..."
                { model | translationMode = ReadOnly }

        ReadOnly ->
            { model | translationMode = Editing }


updateTranslation : TranslationKey -> TranslationValue -> Model -> Model
updateTranslation key value ({ i18nLookup } as model) =
    { model
        | i18nLookup = Dict.insert key value i18nLookup
    }



-- View


view : Model -> Html Msg
view ({ i18nLookup, translationMode } as model) =
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
        , someLabel model []
        , someButton model
            [ if editing then
                Attr.class ""
              else
                onClick ToggleTranslationMode
            ]
        , someLabel model [ Attr.class "--different" ]
        ]


someButton : Model -> List (Attribute Msg) -> Html Msg
someButton model attrs =
    i15d Html.button "Some Button" "some.button" attrs model


someLabel : Model -> List (Attribute Msg) -> Html Msg
someLabel model attrs =
    i15d Html.div "Hello, World!" "some.label" attrs model


toggleModeButton : { a | translationMode : TranslationMode } -> Html Msg
toggleModeButton model =
    Html.button
        [ onClick ToggleTranslationMode
        ]
        [ Html.text "Switch"
        ]


toolbar : { a | translationMode : TranslationMode } -> Html Msg
toolbar model =
    Html.div []
        [ baseStyle
        , toggleModeButton model
        , Html.hr [] []
        ]



-- Helpers


i15d : (List (Attribute Msg) -> List (Html Msg) -> Html Msg) -> String -> TranslationKey -> List (Attribute Msg) -> Model -> Html Msg
i15d element defaultValue key attrs ({ i18nLookup, translationMode } as model) =
    element (i18n key model ++ attrs)
        [ if translationMode == Editing then
            Html.text defaultValue
          else
            Html.text (Intl.lookup defaultValue key i18nLookup)
        ]


i18n : TranslationKey -> { a | translationMode : TranslationMode } -> List (Attribute Msg)
i18n key { translationMode } =
    Intl.i18n key (UpdateTranslation key) translationMode


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

pre {
    background: #eee;
    border: 1px solid #ddd;
}

            """
        ]
