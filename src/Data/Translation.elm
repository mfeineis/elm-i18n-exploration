module Data.Translation exposing (decoder, request)

import Env
import Http
import Intl
import Json.Decode as Decode exposing (Decoder)


decoder : Decoder Intl.Lookup
decoder =
    Intl.decoder


request : List Intl.Locale -> Http.Request Intl.Lookup
request locales =
    let
        route =
            "/" ++ (String.join ";" locales)
    in
    Http.get (Env.translationEndpoint ++ route) Intl.decoder
