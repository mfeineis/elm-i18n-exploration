module Intl
    exposing
        ( Context
        , Msg(..)
        , TranslationKey
        , TranslationMode(..)
        , TranslationValue
        , init
        , translatable
        , vigor
        )

import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Vigor exposing (Recipe, Vigor)


type alias Context =
    { focusedTranslatable : TranslationKey
    , focusedValue : TranslationValue
    , i18nLookup : Dict TranslationKey TranslationValue
    , translationMode : TranslationMode
    }


type Msg
    = FocusTranslatable TranslationKey TranslationValue
    | ToggleTranslationMode
    | UpdateTranslation TranslationKey TranslationValue


type alias TranslationKey =
    String


type alias TranslationValue =
    String


type TranslationMode
    = Editing
    | NotEditing


init : Dict TranslationKey TranslationValue -> Context
init preloaded =
    { focusedTranslatable = ""
    , focusedValue = ""
    , i18nLookup = preloaded
    , translationMode = NotEditing
    }


vigor : Recipe Context Msg ctx msg -> Vigor ctx msg
vigor =
    Vigor.summon
        { subscriptions = \_ -> Sub.none
        , update = update
        , view = \_ -> Html.text ""
        }


update : Msg -> Context -> ( Context, Cmd Msg )
update msg model =
    case msg of
        FocusTranslatable key value ->
            let
                focus model =
                    { model
                        | focusedTranslatable = key
                        , focusedValue = value
                    }
            in
            ( model |> focus, Cmd.none )

        ToggleTranslationMode ->
            model |> toggleTranslationMode

        UpdateTranslation key value ->
            model |> updateTranslation key value


toggleTranslationMode : Context -> ( Context, Cmd Msg )
toggleTranslationMode ({ i18nLookup, translationMode } as model) =
    case translationMode of
        Editing ->
            Debug.log "Done editing..."
                { model | translationMode = NotEditing }
                |> withoutCmd

        NotEditing ->
            { model | translationMode = Editing }
                |> withoutCmd


updateTranslation : TranslationKey -> TranslationValue -> Context -> ( Context, Cmd Msg )
updateTranslation key value ({ i18nLookup } as model) =
    -- TODO: sanitize user input!
    { model
        | i18nLookup = Dict.insert key value i18nLookup
    }
        |> Debug.log ("Sanitized: " ++ value)
        |> withoutCmd


translatable : (List (Attribute Msg) -> List (Html Msg) -> Html Msg) -> String -> TranslationKey -> Context -> List (Attribute Msg) -> Html Msg
translatable element defaultValue key ({ focusedTranslatable, focusedValue, i18nLookup, translationMode } as model) attrs =
    -- TODO: support for multi-line values?
    let
        ( value, focusAttrs ) =
            if focusedTranslatable == key then
                ( focusedValue, [ Attr.class "focused" ] )
            else
                ( lookup defaultValue key i18nLookup, [ Attr.class "" ] )
    in
    element (i18n key value model ++ focusAttrs ++ attrs)
        [ if focusedTranslatable == key && translationMode == Editing then
            -- While the element is focused we don't want Elm to tinker with the node
            Html.text focusedValue
          else
            Html.text (lookup defaultValue key i18nLookup)
        ]


i18n : TranslationKey -> TranslationValue -> Context -> List (Attribute Msg)
i18n key value { translationMode } =
    i18nAttr key (FocusTranslatable "" "") (FocusTranslatable key value) (UpdateTranslation key) translationMode


i18nAttr : TranslationKey -> msg -> msg -> (String -> msg) -> TranslationMode -> List (Attribute msg)
i18nAttr key captureBlur captureFocus mapInput mode =
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

