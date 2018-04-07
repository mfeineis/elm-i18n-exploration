module Intl exposing (TranslationKey, TranslationMode(..), TranslationValue, i18n, lookup)

import Dict exposing (Dict)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)


type alias TranslationKey =
    String


type alias TranslationValue =
    String


type TranslationMode
    = Editing
    | ReadOnly


i18n : TranslationKey -> msg -> msg -> (String -> msg) -> TranslationMode -> List (Attribute msg)
i18n key captureBlur captureFocus mapInput mode =
    let
        editable =
            mode == Editing
    in
    [ Attr.attribute "data-i18n" key
    , if editable then
        Attr.attribute "contenteditable" "true"
      else
        Attr.class ""
    , if editable then
        Events.on "input" (Decode.map mapInput innerTextDecoder)
      else
        Attr.class ""
    , if editable then
        Events.onFocus captureFocus
      else
        Attr.class ""
    , if editable then
        Events.onBlur captureBlur
      else
        Attr.class ""
    ]


lookup : TranslationValue -> TranslationKey -> Dict TranslationKey TranslationValue -> String
lookup defaultValue key lookup =
    case Dict.get key lookup of
        Nothing ->
            defaultValue

        Just value ->
            value



-- Custom decoder is necessary, because the default decoder looks for
-- `event.target.value`, which doesn't exist on e.g. <div>s.
-- See https://github.com/elm-lang/html/issues/24


innerTextDecoder : Decoder String
innerTextDecoder =
    Decode.at [ "target", "innerText" ] Decode.string
