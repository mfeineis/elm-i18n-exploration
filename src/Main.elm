module Main exposing (main)

import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick)
import Intl exposing (TranslationKey, TranslationMode(..))
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


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
    = ToggleTranslationMode


init : ( Model, Cmd Msg )
init =
    ( { translationMode = ReadOnly }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleTranslationMode ->
            model |> toggleTranslationMode |> withoutCmd


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
    Html.div
        [ if translationMode == Editing then
            Attr.class "i18n"
          else
            Attr.class ""
        ]
        [ baseStyle
        , toggleModeButton model
        , Html.div
            (i18n "some.key" model ++ [])
            [ Html.text "Hello, World!"
            ]
        ]


toggleModeButton : { a | translationMode : TranslationMode } -> Html Msg
toggleModeButton model =
    Html.button
        (i18n "some.button" model ++
            [ onClick ToggleTranslationMode
            ]
        )
        [ Html.text "Switch"
        ]



-- Helpers


i18n : TranslationKey -> { a | translationMode : TranslationMode } -> List (Attribute msg)
i18n key { translationMode } =
    Intl.i18n key translationMode


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

.i18n [data-i18n] {
    border: 1px solid red;
}

pre {
    background: #eee;
    border: 1px solid #ddd;
}

            """
        ]
