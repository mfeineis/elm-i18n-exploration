module Intl
    exposing
        ( Lookup
        , TranslationKey
        , TranslationMode(..)
        , TranslationValue
        , empty
        , encode
        , get
        , i18n
        , insert
        )

import Dict exposing (Dict)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type alias Lookup =
    Dict TranslationKey TranslationValue


type alias TranslationKey =
    String


type alias TranslationValue =
    String


type TranslationMode
    = Editing
    | NotEditing


empty : Lookup
empty =
    Dict.empty


insert : TranslationKey -> TranslationValue -> Lookup -> Lookup
insert key value lookup =
    Dict.insert key value lookup


encode : Lookup -> Value
encode =
    encodeDict identity Encode.string


encodeDict : (comparable -> String) -> (v -> Value) -> Dict comparable v -> Value
encodeDict toKey toValue dict =
    Dict.toList dict
        |> List.map (\( key, value ) -> ( toKey key, toValue value ))
        |> Encode.object


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
        Events.on "input" (Decode.map mapInput editableContentDecoder)
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


get : TranslationValue -> TranslationKey -> Lookup -> String
get defaultValue key lookup =
    case Dict.get key lookup of
        Nothing ->
            defaultValue

        Just value ->
            value



-- Custom decoder is necessary, because the default decoder looks for
-- `event.target.value`, which doesn't exist on e.g. <div>s.
-- See https://github.com/elm-lang/html/issues/24


editableContentDecoder : Decoder String
editableContentDecoder =
    Decode.oneOf
        [ Decode.at [ "target", "value" ] Decode.string
        , Decode.at [ "target", "innerText" ] Decode.string
        ]
