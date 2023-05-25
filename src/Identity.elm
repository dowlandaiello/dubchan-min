module Identity exposing (..)

import Html exposing (Html, option, select, text)
import Json.Decode as JD
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import Json.Encode.Optional as Opt
import List as L
import Msg exposing (Msg)
import Sha256 exposing (sha256)
import String as St


type alias Identity =
    { tripcode : String
    , pubKey : String
    , privKey : String
    , encPubKey : Maybe String
    , encPrivKey : Maybe String
    }


type alias SignatureRequest =
    { privKey : String
    , encPrivKey : Maybe String
    , msg : JE.Value
    }


signatureRequestEncoder : SignatureRequest -> JE.Value
signatureRequestEncoder req =
    [ ( "privKey", req.privKey ) |> Opt.field JE.string, ( "msg", req.msg ) |> Opt.field identity, ( "encPrivKey", req.encPrivKey ) |> Opt.optionalField JE.string ] |> Opt.objectMaySkip


identityHash : String -> String
identityHash =
    sha256


identityShortcode : String -> String
identityShortcode =
    identityHash >> St.left 5


identityDecoder : JD.Decoder Identity
identityDecoder =
    JD.succeed Identity
        |> andMap (JD.field "tripcode" JD.string)
        |> andMap (JD.field "pubKey" JD.string)
        |> andMap (JD.field "privKey" JD.string)
        |> andMap (JD.maybe (JD.field "encPrivKey" JD.string))
        |> andMap (JD.maybe (JD.field "encPubKey" JD.string))


identityEncoder : Identity -> JE.Value
identityEncoder iden =
    [ ( "tripcode", iden.tripcode ) |> Opt.field JE.string, ( "pubKey", iden.pubKey ) |> Opt.field JE.string, ( "privKey", iden.pubKey ) |> Opt.field JE.string, ( "encPrivKey", iden.encPrivKey ) |> Opt.optionalField JE.string, ( "encPubKey", iden.encPubKey ) |> Opt.optionalField JE.string ] |> Opt.objectMaySkip


viewIdentitySelector : List Identity -> Html Msg
viewIdentitySelector =
    L.map (.pubKey >> identityShortcode >> text >> L.singleton >> option []) >> select []
