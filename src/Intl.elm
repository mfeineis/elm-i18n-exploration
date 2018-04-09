module Intl
    exposing
        ( Language
        , Locale
        , Lookup
        , TranslationKey
        , TranslationMode(..)
        , TranslationValue
        , decoder
        , empty
        , encode
        , get
        , i18n
        , insert
        , locale
        , localeDecoder
        )

import Dict exposing (Dict)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type alias Language = String


type alias Locale = String


type alias Lookup =
    { language : Language
    , locale : Locale
    , lookup : Dict TranslationKey TranslationValue
    , supportedLocales : List Locale
    }


type alias TranslationKey =
    String


type alias TranslationValue =
    String


type TranslationMode
    = Editing
    | NotEditing


defaultLanguage : Locale
defaultLanguage = "en"


defaultLocale : Locale
defaultLocale = "en-US"


empty : Lookup
empty =
    { language = defaultLanguage
    , locale = defaultLocale
    , lookup = Dict.empty
    , supportedLocales = [defaultLocale]
    }


insert : TranslationKey -> TranslationValue -> Lookup -> Lookup
insert key value ({ lookup } as it) =
    { it | lookup = Dict.insert key value lookup }


decoder : Decoder Lookup
decoder =
    Decode.map4 Lookup
        (Decode.field "language" languageDecoder)
        (Decode.field "locale" localeDecoder)
        (Decode.field "lookup" (Decode.dict Decode.string))
        (Decode.field "supportedLocales" (Decode.list localeDecoder))


locale : String -> Locale
locale locale =
    locale


encode : Lookup -> Value
encode { language, locale, lookup, supportedLocales } =
    Encode.object
        [ ( "language", Encode.string language )
        , ( "locale", Encode.string locale )
        , ( "lookup", encodeDict identity Encode.string lookup )
        , ( "supportedLocales", supportedLocales |> List.map Encode.string |> Encode.list )
        ]

encodeDict : (comparable -> String) -> (v -> Value) -> Dict comparable v -> Value
encodeDict toKey toValue dict =
    Dict.toList dict
        |> List.map (\( key, value ) -> ( toKey key, toValue value ))
        |> Encode.object


get : TranslationValue -> TranslationKey -> Lookup -> String
get defaultValue key { lookup } =
    case Dict.get key lookup of
        Nothing ->
            defaultValue

        Just value ->
            value



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



-- Custom decoder is necessary, because the default decoder looks for
-- `event.target.value`, which doesn't exist on e.g. <div>s.
-- See https://github.com/elm-lang/html/issues/24


editableContentDecoder : Decoder String
editableContentDecoder =
    Decode.oneOf
        [ Decode.at [ "target", "value" ] Decode.string
        , Decode.at [ "target", "innerText" ] Decode.string
        ]


-- Helpers


languageDecoder : Decoder Language
languageDecoder =
    Decode.string


localeDecoder : Decoder Locale
localeDecoder =
    Decode.string


