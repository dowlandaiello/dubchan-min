port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Encode as JE
import Maybe as M
import Msg exposing (Msg(..))
import Post exposing (CommentSubmission, MultimediaKind(..), Post, Submission, commentDecoder, commentEncoder, commentFromSubmission, fromSubmission, postDecoder, postEncoder, pushComment, setContent, setContentKind, setText, setTitle, viewCommentArea, viewPost, viewPostComments, viewPosts, viewSubmitPost)
import Route exposing (..)
import Time
import Url
import Url.Parser exposing (parse)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , feed : List Post
    , viewing : Maybe Post
    , submission : Submission
    , commentSubmission : CommentSubmission
    , time : Time.Posix
    }


setSubmission : Submission -> Model -> Model
setSubmission s m =
    { m | submission = s }


setCommentSubmission : CommentSubmission -> Model -> Model
setCommentSubmission s m =
    { m | commentSubmission = s }


setViewing : Post -> Model -> Model
setViewing p m =
    { m | viewing = Just p }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case
        parse routeParser url
    of
        Just (Route.Post p) ->
            update (SelectPost (Just p)) (Model key url [] Nothing (Submission "" "" "" Image) (CommentSubmission "") (Time.millisToPosix 0))

        Nothing ->
            ( Model key url [] Nothing (Submission "" "" "" Image) (CommentSubmission "") (Time.millisToPosix 0), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( { model | time = t }, Cmd.none )

        ClickLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External url ->
                    ( model, Nav.load url )

        ChangeUrl url ->
            case
                M.map
                    (\route ->
                        case route of
                            Route.Post p ->
                                SelectPost (Just p)
                    )
                    (parse routeParser url)
            of
                Just m ->
                    update m model

                Nothing ->
                    ( model, Cmd.none )

        SelectPost p ->
            case p of
                Just post ->
                    ( model, loadPost post )

                Nothing ->
                    ( { model | viewing = Nothing }, Cmd.none )

        PostLoaded p ->
            case JD.decodeValue postDecoder p of
                Ok post ->
                    ( { model | viewing = Just post }, Cmd.none )

                otherwise ->
                    ( model, Cmd.none )

        PostAdded p ->
            case JD.decodeValue postDecoder p of
                Ok post ->
                    ( { model | feed = post :: model.feed }, Cmd.none )

                otherwise ->
                    ( model, Cmd.none )

        ChangeSubTitle s ->
            ( model |> setSubmission (model.submission |> setTitle s), Cmd.none )

        ChangeSubText s ->
            ( model |> setSubmission (model.submission |> setText s), Cmd.none )

        ChangeSubContent s ->
            ( model |> setSubmission (model.submission |> setContent s), Cmd.none )

        SetSubContentImage ->
            ( model |> setSubmission (model.submission |> setContentKind Image), Cmd.none )

        SetSubContentVideo ->
            ( model |> setSubmission (model.submission |> setContentKind Video), Cmd.none )

        SubmitPost ->
            ( model |> setSubmission (Submission "" "" "" Image), model.submission |> fromSubmission model.time |> postEncoder |> submitPost )

        ChangeSubCommentText s ->
            ( model |> setCommentSubmission (CommentSubmission s), Cmd.none )

        SubmitComment ->
            case model.viewing of
                Just viewing ->
                    ( model |> setCommentSubmission (CommentSubmission ""), model.commentSubmission |> commentFromSubmission viewing.id model.time |> commentEncoder |> submitComment )

                Nothing ->
                    ( model |> setCommentSubmission (CommentSubmission ""), Cmd.none )

        CommentAdded c ->
            case JD.decodeValue commentDecoder c of
                Ok comment ->
                    case model.viewing of
                        Just viewing ->
                            ( model |> setViewing (viewing |> pushComment comment), Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                otherwise ->
                    ( model, Cmd.none )


port loadPost : String -> Cmd msg


port postLoaded : (JE.Value -> msg) -> Sub msg


port postIn : (JE.Value -> msg) -> Sub msg


port commentIn : (JE.Value -> msg) -> Sub msg


port submitPost : JE.Value -> Cmd msg


port submitComment : JE.Value -> Cmd msg


view : Model -> Browser.Document Msg
view model =
    { title = "DubChan"
    , body =
        let
            home =
                [ div [ class "feed" ]
                    [ div [ class "logo" ] [ img [ src "/logo.png" ] [], div [ class "logoText" ] [ h1 [] [ text "DubChan" ], p [] [ text "Anonymous. Unmoderated." ] ] ]
                    , viewSubmitPost model.submission.contentKind
                    , viewPosts
                        model.feed
                    ]
                ]
        in
        case model.viewing of
            Just viewing ->
                div [ class "viewer" ] [ div [ class "viewerBody" ] [ div [ class "navigation" ] [ img [ src "/back.svg", onClick (SelectPost Nothing) ] [] ], viewPost viewing, viewCommentArea, viewPostComments (M.withDefault [] viewing.comments) ] ] :: home

            Nothing ->
                home
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ postLoaded PostLoaded, postIn PostAdded, commentIn CommentAdded, Time.every 1 Tick ]


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickLink
        , onUrlChange = ChangeUrl
        }