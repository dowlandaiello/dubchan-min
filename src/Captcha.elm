module Captcha exposing (..)

import Json.Decode as JD exposing (Decoder, field, string)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import Sha256 exposing (sha256)


type alias Captcha =
    { answer : String
    , data : String
    }


type alias CaptchaMsg =
    { post : String
    , captcha : Captcha
    }


hash : Captcha -> Captcha
hash c =
    let
        a =
            c.answer
    in
    { c | answer = sha256 a }


captchaMsgDecoder : Decoder CaptchaMsg
captchaMsgDecoder =
    JD.succeed CaptchaMsg |> andMap (field "post" string) |> andMap (field "captcha" captchaDecoder)


captchaDecoder : Decoder Captcha
captchaDecoder =
    JD.succeed Captcha
        |> andMap (field "answer" string)
        |> andMap (field "data" string)


captchaEncoder : Captcha -> JE.Value
captchaEncoder c =
    JE.object
        [ ( "answer", JE.string c.answer ), ( "data", JE.string c.data ) ]


isValidCaptcha : String -> Captcha -> Bool
isValidCaptcha c1 c2 =
    c1 |> sha256 |> (==) c2.answer
