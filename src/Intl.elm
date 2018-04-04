module Intl exposing (TranslationKey, TranslationMode(..), i18n)

import Html exposing (Attribute)
import Html.Attributes as Attr


type alias TranslationKey =
    String


type TranslationMode
    = Editing
    | ReadOnly


i18n : TranslationKey -> TranslationMode -> List (Attribute msg)
i18n key mode =
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
        Attr.attribute "oninput"
            """

console.log(`oninput`, event);
const it = document.createElement(`pre`);
it.innerHTML = event.target.innerHTML;
document.body.appendChild(it);

            """
      else
        Attr.class ""
    ]
