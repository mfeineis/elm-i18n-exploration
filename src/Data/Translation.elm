module Data.Translation exposing (decoder, request)

import Env
import Http
import Intl
import Json.Decode as Decode exposing (Decoder)


decoder : Decoder Intl.Lookup
decoder =
    Intl.decoder


request : Http.Request Intl.Lookup
request =
    Http.get Env.translationEndpoint Intl.decoder
