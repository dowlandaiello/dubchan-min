module Nav exposing (..)

import Html exposing (Html, div, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List as L
import Msg exposing (..)


type alias NavEntry =
    { label : String
    , icon : String
    , iconActive : String
    , tab : Tab
    }


navEntries : List NavEntry
navEntries =
    [ { label = "Home", icon = "/home.svg", iconActive = "/home_active.svg", tab = Feed }, { label = "Settings", icon = "/settings.svg", iconActive = "/settings_active.svg", tab = Settings } ]


viewNavEntry : Tab -> NavEntry -> Html Msg
viewNavEntry active ent =
    div
        ([ Just (class "navItem")
         , Just (onClick (ChangeTabViewing ent.tab))
         , if active == ent.tab then
            Just (class "active")

           else
            Nothing
         ]
            |> L.filterMap identity
        )
        [ img
            [ class "navIcon"
            , src
                (if active == ent.tab then
                    ent.iconActive

                 else
                    ent.icon
                )
            ]
            []
        , p [ class "navLabel" ] [ text ent.label ]
        ]


viewNavEntries : List NavEntry -> Tab -> Html Msg
viewNavEntries ent active =
    L.map (viewNavEntry active) ent |> div [ class "navBar" ]


viewNavigator : Tab -> Html Msg
viewNavigator =
    viewNavEntries navEntries
