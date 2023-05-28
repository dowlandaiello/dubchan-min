module Mail exposing (..)

import Dict as D
import Html exposing (Html, div, h1, img, input, p, text, textarea)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Identity exposing (Identity, identityFullname, identityShortcode)
import List as L
import Maybe as M
import Model exposing (..)
import Msg exposing (Conversation, Msg(..))
import Post exposing (MultimediaKind(..), viewIdSelector, viewPostText, viewTimestamp)
import Sha256 exposing (sha256)
import String as S


viewMailbox : Bool -> Mailbox -> Html Msg
viewMailbox active mailbox =
    div
        ([ Just (class "mailbox")
         , Just (onClick (OpenConvo mailbox.info))
         , if active then
            Just (class "active")

           else
            Nothing
         ]
            |> L.filterMap identity
        )
        (let
            senderLine =
                p [ class "mailboxSender" ] [ text ((mailbox.info.tripcode |> M.map ((++) "@") |> M.withDefault "") ++ "#" ++ identityShortcode mailbox.info.pubKey) ]
         in
         case mailbox.messages |> L.head of
            Just headline ->
                [ div [ class "mailboxHeaderLine" ] [ senderLine, headline.timestamp |> viewTimestamp ]
                , p [ class "mailboxHeadline" ] [ text headline.text ]
                ]

            Nothing ->
                [ senderLine ]
        )


viewMailboxes : String -> List Mailbox -> Html Msg
viewMailboxes active =
    L.map (\mailbox -> viewMailbox (active == sha256 mailbox.info.encPubKey) mailbox) >> div [ class "mailboxes" ]


viewChatMessage : Message -> Html Msg
viewChatMessage m =
    div [ class "message" ]
        [ div [ class "statusLine" ]
            [ p [ class "authorLabel" ]
                [ text (identityFullname m.tripcode m.pubKey) ]
            , viewTimestamp m.timestamp
            ]
        , viewPostText
            m.text
        ]


viewChatMessages : List Message -> Html Msg
viewChatMessages =
    L.map viewChatMessage >> div [ class "messagesList" ]


viewChatInput : List Identity -> Identity -> String -> String -> MultimediaKind -> Html Msg
viewChatInput identities activeIdentity bodyText attachment activeMediaType =
    div [ class "chatInput" ]
        [ div [ class "submitLine" ]
            [ viewIdSelector False
                identities
                (Just
                    activeIdentity
                )
                (\s ->
                    if S.isEmpty s then
                        ChangeSubMessageTripcode Nothing

                    else
                        ChangeSubMessageTripcode (Just s)
                )
            ]
        , textarea [ placeholder "Message text", value bodyText, onInput ChangeSubMessageText ] []
        , div [ class "submitLine" ]
            [ input [ placeholder "Link an attachment", value attachment, onInput ChangeSubMessageContent ] []
            , div [ class "mediaSelector" ]
                [ p
                    ([ if activeMediaType == Image then
                        Just (class "active")

                       else
                        Nothing
                     , Just (onClick SetSubMessageImage)
                     ]
                        |> L.filterMap identity
                    )
                    [ text "Image" ]
                , p
                    ([ if activeMediaType == Video then
                        Just (class "active")

                       else
                        Nothing
                     , Just (onClick SetSubMessageVideo)
                     ]
                        |> L.filterMap identity
                    )
                    [ text "Video" ]
                ]
            , img [ src "/send.svg", class "icon", onClick SubmitMessage ] []
            ]
        ]


viewChatArea : Model -> Html Msg
viewChatArea model =
    let
        messages =
            case D.get model.mailInfo.activeConvo model.mailInfo.conversations of
                Just activeConvo ->
                    viewChatMessages (activeConvo.messages |> L.sortBy .timestamp |> L.reverse)

                Nothing ->
                    text ""
    in
    div [ class "messagesArea" ]
        [ messages
        , viewChatInput model.settingsInfo.identities (model.subInfo.subIdentity |> M.withDefault (model.settingsInfo.identities |> L.head |> M.withDefault (Identity "" "" "" Nothing Nothing))) model.subInfo.messageSubmission.text model.subInfo.messageSubmission.content model.subInfo.messageSubmission.contentKind
        ]


viewMail : Model -> Html Msg
viewMail model =
    div [ class "fullPage" ]
        [ h1 [ class "pageTitle" ]
            [ text "Messages" ]
        , div
            [ class "mailWorkspace" ]
            [ D.values model.mailInfo.conversations |> viewMailboxes model.mailInfo.activeConvo
            , if D.member model.mailInfo.activeConvo model.mailInfo.conversations then
                viewChatArea model

              else
                text ""
            ]
        ]
