module Identity exposing (..)

import Html exposing (Html, option, select, text)
import Json.Decode as JD
import Json.Encode as JE
import List as L
import Msg exposing (Msg)
import Sha256 exposing (sha256)
import String as St


type alias Identity =
    { tripcode : String
    , pubKey : String
    , privKey : String
    }


type alias SignatureRequest =
    { privKey : String
    , msg : JE.Value
    }


signatureRequestEncoder : SignatureRequest -> JE.Value
signatureRequestEncoder req =
    JE.object [ ( "privKey", JE.string req.privKey ), ( "msg", req.msg ) ]


identityHash : String -> String
identityHash =
    sha256


identityShortcode : Identity -> String
identityShortcode =
    .pubKey >> identityHash >> St.left 5


identityDecoder : JD.Decoder Identity
identityDecoder =
    JD.map3 Identity (JD.field "tripcode" JD.string) (JD.field "pubKey" JD.string) (JD.field "privKey" JD.string)


identityEncoder : Identity -> JE.Value
identityEncoder iden =
    JE.object [ ( "tripcode", JE.string iden.tripcode ), ( "pubKey", JE.string iden.pubKey ), ( "privKey", JE.string iden.pubKey ) ]


viewIdentitySelector : List Identity -> Html Msg
viewIdentitySelector =
    L.map (identityShortcode >> text >> L.singleton >> option []) >> select []
