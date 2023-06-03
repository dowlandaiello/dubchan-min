module Tag exposing (..)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List as L
import Msg exposing (Msg(..))
import Set as S


tags : List String
tags =
    [ "UW", "Lifting", "LGBT", "NSFW" ]


viewTagSelector : S.Set String -> Html Msg
viewTagSelector activeTags =
    tags
        |> L.map
            (\tag ->
                div
                    ([ class "tag" |> Just
                     , if S.member tag activeTags then
                        class "active" |> Just

                       else
                        Nothing
                     , if S.member tag activeTags then
                        RemoveSubTag tag |> onClick |> Just

                       else
                        AddSubTag tag |> onClick |> Just
                     ]
                        |> L.filterMap identity
                    )
                    [ text tag ]
            )
        |> div [ class "tagList" ]


viewTagsArea : S.Set String -> Html Msg
viewTagsArea =
    viewTagSelector
