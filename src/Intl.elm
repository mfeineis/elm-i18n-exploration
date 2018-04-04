module Intl exposing (TranslationKey, TranslationMode(..), i18n)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)


type alias TranslationKey =
    String


type TranslationMode
    = Editing
    | ReadOnly


i18n : TranslationKey -> (String -> msg) -> TranslationMode -> List (Attribute msg)
i18n key tagger mode =
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
        Events.on "input" (Decode.map tagger innerHtmlDecoder)
      else
        Attr.class ""
    ]


-- Custom decoder is necessary, because the default decoder looks for
-- `event.target.value`, which doesn't exist on e.g. <div>s.
-- See https://github.com/elm-lang/html/issues/24

innerHtmlDecoder : Decoder String
innerHtmlDecoder =
    Decode.at ["target", "innerHTML"] Decode.string
