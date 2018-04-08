module Data.Translation exposing (decoder, request)

import Http
import Intl
import Json.Decode as Decode exposing (Decoder)


decoder : Decoder Intl.Lookup
decoder =
    Decode.dict Decode.string


request : Http.Request Intl.Lookup
request =
    Http.get "/api/i18n" decoder
