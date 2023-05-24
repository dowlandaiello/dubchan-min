module Model exposing (..)

import Browser.Navigation as Nav
import Captcha exposing (Captcha, captchaDecoder, captchaMsgDecoder, hash, isValidCaptcha)
import Dict as D
import Html exposing (Html, canvas, div, h1, img, input, p, text)
import Html.Attributes exposing (class, id, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Identity exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import List as L
import List.Extra as LE
import Maybe as M
import Msg exposing (Msg(..), Tab(..))
import Nav exposing (viewNavigator)
import Post exposing (Comment, CommentSubmission, MultimediaKind(..), Post, Submission, commentDecoder, commentEncoder, commentFromSubmission, commentId, descending, fromSubmission, getChunkTime, isValidHash, postChunkDecoder, postDecoder, postEncoder, postId, pushComment, setContent, setContentKind, setText, setTitle, subContentValid, submissionFromComment, submissionFromPost, viewCommentArea, viewCommentText, viewExpandableMultimedia, viewMultimedia, viewPost, viewSubmitPost, viewTimestamp)
import Set as S
import Sha256 exposing (sha256)
import String
import Time
import Url
import Url.Parser exposing (parse, query)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , subInfo : SubmissionInfo
    , feedInfo : FeedInfo
    , navInfo : NavigationInfo
    , settingsInfo : SettingsInfo
    , time : Time.Posix
    }


type alias NavigationInfo =
    { tabViewing : Tab
    }


type alias SettingsInfo =
    { identities : List Identity
    }


type alias SubmissionInfo =
    { submission : Submission
    , commentSubmission : CommentSubmission
    , submitting : Maybe Post
    , commentSubmitting : Maybe Comment
    , submissionFeedback : String
    , head : Maybe Post
    , captchaHead : Captcha
    , subIdentity : Maybe Identity
    }


type alias FeedInfo =
    { searchQuery : String
    , visibleMedia : S.Set String
    , expandedMedia : S.Set String
    , blurImages : Bool
    , lastChunk : Int
    , lastChunkPaginated : Int
    , qotd : String
    , feed : D.Dict String Post
    , feedDisplay : D.Dict Int (List Post)
    , comments : D.Dict String (D.Dict String Comment)
    , captchas : D.Dict String Captcha
    , viewing : Maybe Post
    , hidden : S.Set String
    }


settingsDecoder : JD.Decoder SettingsInfo
settingsDecoder =
    JD.map SettingsInfo (JD.field "identities" (JD.list identityDecoder))


settingsEncoder : SettingsInfo -> JE.Value
settingsEncoder s =
    JE.object [ ( "identities", JE.list identityEncoder s.identities ) ]


setSubmissionInfo : SubmissionInfo -> Model -> Model
setSubmissionInfo s m =
    { m | subInfo = s }


setSettingsInfo : SettingsInfo -> Model -> Model
setSettingsInfo s m =
    { m | settingsInfo = s }


setCaptchaHead : Captcha -> SubmissionInfo -> SubmissionInfo
setCaptchaHead c m =
    { m | captchaHead = c }


setQotd : String -> FeedInfo -> FeedInfo
setQotd q m =
    { m | qotd = q }


setFeedInfo : FeedInfo -> Model -> Model
setFeedInfo s m =
    { m | feedInfo = s }


setVisibleMedia : S.Set String -> FeedInfo -> FeedInfo
setVisibleMedia s m =
    { m | visibleMedia = s }


setCaptcha : String -> Captcha -> FeedInfo -> FeedInfo
setCaptcha p c m =
    let
        captchas =
            m.captchas
    in
    { m | captchas = D.insert p c captchas }


setBlurImages : Bool -> FeedInfo -> FeedInfo
setBlurImages b m =
    { m | blurImages = b }


setCaptchaAnswer : String -> Post -> Post
setCaptchaAnswer s p =
    { p | captchaAnswer = Just s }


setCommentCaptchaAnswer : String -> Comment -> Comment
setCommentCaptchaAnswer s c =
    { c | captchaAnswer = Just s }


setLastChunk : Int -> FeedInfo -> FeedInfo
setLastChunk c m =
    { m | lastChunk = c }


setLastChunkPaginated : Int -> FeedInfo -> FeedInfo
setLastChunkPaginated c m =
    { m | lastChunkPaginated = c }


setSubmission : Submission -> SubmissionInfo -> SubmissionInfo
setSubmission s m =
    { m | submission = s }


setSubmitting : Maybe Post -> SubmissionInfo -> SubmissionInfo
setSubmitting s m =
    { m | submitting = s }


setCommentSubmitting : Maybe Comment -> SubmissionInfo -> SubmissionInfo
setCommentSubmitting s m =
    { m | commentSubmitting = s }


setCommentSubmission : CommentSubmission -> SubmissionInfo -> SubmissionInfo
setCommentSubmission s m =
    { m | commentSubmission = s }


setSubmissionFeedback : String -> SubmissionInfo -> SubmissionInfo
setSubmissionFeedback s m =
    { m | submissionFeedback = s }


setSubIdentity : Maybe Identity -> SubmissionInfo -> SubmissionInfo
setSubIdentity s m =
    { m | subIdentity = s }


setSearchQuery : String -> FeedInfo -> FeedInfo
setSearchQuery s m =
    { m | searchQuery = s }


setViewing : Maybe Post -> FeedInfo -> FeedInfo
setViewing p m =
    { m | viewing = p }


setExpanded : S.Set String -> FeedInfo -> FeedInfo
setExpanded e m =
    { m | expandedMedia = e }


setHead : Maybe Post -> SubmissionInfo -> SubmissionInfo
setHead p m =
    { m | head = p }


setHidden : S.Set String -> FeedInfo -> FeedInfo
setHidden h m =
    { m | hidden = h }


setComments : D.Dict String (D.Dict String Comment) -> FeedInfo -> FeedInfo
setComments c m =
    { m | comments = c }


setNavInfo : NavigationInfo -> Model -> Model
setNavInfo nav model =
    { model | navInfo = nav }


setTabViewing : Tab -> NavigationInfo -> NavigationInfo
setTabViewing t nav =
    { nav | tabViewing = t }


addComment : Comment -> Model -> Model
addComment c m =
    if c.text == "" then
        m

    else
        m
            |> setFeedInfo
                (m.feedInfo
                    |> setComments
                        (D.update c.parent
                            (\maybeComments ->
                                case maybeComments of
                                    Just comments ->
                                        Just (D.insert c.id c comments)

                                    Nothing ->
                                        Just (D.fromList [ ( c.id, c ) ])
                            )
                            m.feedInfo.comments
                        )
                )


addPost : Int -> Post -> Model -> Model
addPost chunk p m =
    let
        display =
            m.feedInfo.feedDisplay
    in
    let
        feedInfo =
            m.feedInfo
    in
    m
        |> setFeedInfo
            { feedInfo
                | feed = D.insert p.id p m.feedInfo.feed
                , feedDisplay =
                    let
                        newChunks =
                            case D.get chunk display of
                                Just chunkItems ->
                                    p :: chunkItems

                                Nothing ->
                                    [ p ]
                    in
                    D.insert chunk newChunks display
            }
        |> setSubmissionInfo
            (m.subInfo
                |> setHead
                    (case m.subInfo.head of
                        Just head ->
                            if p.timestamp > head.timestamp then
                                Just p

                            else
                                Just head

                        Nothing ->
                            Just p
                    )
            )


toggleHidden : String -> Model -> Model
toggleHidden parent m =
    let
        hidden =
            m.feedInfo.hidden
    in
    if S.member parent m.feedInfo.hidden then
        m
            |> setFeedInfo (m.feedInfo |> setHidden (S.remove parent m.feedInfo.hidden))

    else
        m |> setFeedInfo (m.feedInfo |> setHidden (S.insert parent m.feedInfo.hidden))


commentsFor : String -> Model -> Maybe (List Comment)
commentsFor s model =
    D.get s model.feedInfo.comments |> M.map D.values |> M.map (L.sortWith descending)


allCommentsFor : String -> Model -> Maybe (List Comment)
allCommentsFor s model =
    let
        roots =
            D.get s model.feedInfo.comments
    in
    roots |> M.map D.values |> M.map (L.concatMap (\comment -> comment :: M.withDefault [] (allCommentsFor comment.id model)))


youngestCommentFor : String -> Model -> Int
youngestCommentFor s model =
    allCommentsFor s model |> M.map (L.sortWith descending) |> M.andThen L.head |> M.map (\comment -> comment.timestamp) |> M.withDefault 0


sortActivity model a b =
    case compare (max (youngestCommentFor a.id model) a.timestamp) (max (youngestCommentFor b.id model) b.timestamp) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT
