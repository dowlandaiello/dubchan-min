port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, img, p, text)
import Html.Attributes exposing (class, src)
import Json.Decode as JD
import Json.Encode as JE
import Maybe as M
import Msg exposing (Msg(..))
import Post exposing (MultimediaKind(..), Post, Submission, fromSubmission, postDecoder, postEncoder, setContent, setContentKind, setText, setTitle, viewPosts, viewSubmitPost)
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
    , time : Time.Posix
    }


setSubmission : Submission -> Model -> Model
setSubmission s m =
    { m | submission = s }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case
        parse routeParser url
    of
        Just (Route.Post p) ->
            update (SelectPost p) (Model key url [] Nothing (Submission "" "" "" Image) (Time.millisToPosix 0))

        Nothing ->
            ( Model key url [] Nothing (Submission "" "" "" Image) (Time.millisToPosix 0), Cmd.none )


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
                                SelectPost p
                    )
                    (parse routeParser url)
            of
                Just m ->
                    update m model

                Nothing ->
                    ( model, Cmd.none )

        SelectPost p ->
            ( model, loadPost p )

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


port loadPost : String -> Cmd msg


port postLoaded : (JE.Value -> msg) -> Sub msg


port postIn : (JE.Value -> msg) -> Sub msg


port submitPost : JE.Value -> Cmd msg


view : Model -> Browser.Document Msg
view model =
    { title = "DubChan"
    , body =
        [ div [ class "feed" ]
            [ div [ class "logo" ] [ img [ src "/logo.png" ] [], div [ class "logoText" ] [ h1 [] [ text "DubChan" ], p [] [ text "Free speech. Boundless." ] ] ]
            , viewSubmitPost model.submission.contentKind
            , viewPosts
                model.feed
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ postLoaded PostLoaded, postIn PostAdded, Time.every 1 Tick ]


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
