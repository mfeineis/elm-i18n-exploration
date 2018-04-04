module Main exposing (main)

import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick)
import Intl exposing (TranslationKey, TranslationMode(..))
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
    { translationMode : TranslationMode
    }


type Msg
    = UpdateTranslation TranslationKey String
    | ToggleTranslationMode


init : ( Model, Cmd Msg )
init =
    ( { translationMode = ReadOnly }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleTranslationMode ->
            model |> toggleTranslationMode |> withoutCmd

        UpdateTranslation key value ->
            Debug.log ("UpdateTranslation " ++ key ++ ": " ++ value)
                (model |> withoutCmd)


toggleTranslationMode : { a | translationMode : TranslationMode } -> { a | translationMode : TranslationMode }
toggleTranslationMode ({ translationMode } as model) =
    case translationMode of
        Editing ->
            { model | translationMode = ReadOnly }

        ReadOnly ->
            { model | translationMode = Editing }



-- View


view : Model -> Html Msg
view ({ translationMode } as model) =
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
        , Html.div
            (i18n "some.label" model ++ [])
            [ Html.text "Hello, World!"
            ]
        , Html.button
            (i18n "some.button" model
                ++ [ if editing then
                        Attr.class ""
                     else
                        onClick ToggleTranslationMode
                   ]
            )
            [ Html.text "Some Button"
            ]
        ]


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
