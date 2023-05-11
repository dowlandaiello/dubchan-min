module Captcha exposing (..)

import Json.Decode as JD exposing (Decoder, field, string)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import Sha256 exposing (sha256)


type alias Captcha =
    { answer : String
    , data : String
    }


hash : Captcha -> Captcha
hash c =
    let
        a =
            c.answer
    in
    { c | answer = sha256 a }


captchaDecoder : Decoder Captcha
captchaDecoder =
    JD.succeed Captcha
        |> andMap (field "answer" string)
        |> andMap (field "data" string)


captchaEncoder : Captcha -> JE.Value
captchaEncoder c =
    JE.object
        [ ( "answer", JE.string c.answer ), ( "data", JE.string c.data ) ]
