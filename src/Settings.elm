module Settings exposing (..)

import Html exposing (Html, div, h1, h2, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Identity exposing (Identity, identityHash)
import List as L
import Model exposing (Model)
import Msg exposing (Msg(..))
import String as St


viewAboutSection : Html Msg
viewAboutSection =
    div [ class "section" ]
        [ h2 [ class "sectionHeader" ] [ text "App Info" ]
        , p [] [ text "App Version: 0.1.94" ]
        , p [] [ text "Last Updated: 5/31/23" ]
        ]


viewIdentity : Identity -> Html Msg
viewIdentity iden =
    let
        shorthand =
            iden.pubKey |> identityHash |> St.left 5
    in
    div [ class "identityLabel", onClick (RemoveIdentity iden.pubKey) ] [ img [ src "/close.svg" ] [], p [] [ text (iden.tripcode ++ "#" ++ shorthand) ] ]


viewIdentities : Model -> Html Msg
viewIdentities m =
    div [ class "section" ] [ h2 [ class "sectionHeader" ] [ text "Manage Identities" ], div [ class "identitiesList" ] (m.settingsInfo.identities |> L.map viewIdentity), p [ class "newIdentity", onClick GenerateIdentity ] [ text "New ID" ] ]


viewSettings : Model -> Html Msg
viewSettings m =
    div [ class "fullPage" ]
        [ h1 [ class "pageTitle" ] [ text "Settings" ]
        , viewAboutSection
        , viewIdentities m
        ]
