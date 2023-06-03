module Model exposing (..)

import Browser.Navigation as Nav
import Captcha exposing (Captcha, captchaDecoder, captchaMsgDecoder, hash, isValidCaptcha)
import Dict as D
import Flip exposing (flip)
import Html exposing (Html, canvas, div, h1, img, input, p, text)
import Html.Attributes exposing (class, id, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Identity exposing (..)
import Json.Decode as JD exposing (field, int, maybe, nullable, string)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import Json.Encode.Optional as Opt
import List as L
import List.Extra as LE
import Maybe as M
import Msg exposing (Conversation, Msg(..), Tab(..))
import Nav exposing (viewNavigator)
import Post exposing (Comment, CommentSubmission, Multimedia, MultimediaKind(..), Post, Submission, commentDecoder, commentEncoder, commentFromSubmission, commentId, descending, fromSubmission, getChunkTime, isValidHash, multimediaDecoder, multimediaEncoder, postChunkDecoder, postDecoder, postEncoder, postId, pushComment, setContent, setContentKind, setText, setTitle, subContentValid, submissionFromComment, submissionFromPost, viewCommentArea, viewCommentText, viewExpandableMultimedia, viewMultimedia, viewPost, viewSubmitPost, viewTimestamp)
import Set as S
import Sha256 exposing (sha256)
import String
import Theme exposing (Theme, defaultTheme, themeDecoder, themes)
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
    , mailInfo : MailInfo
    , time : Time.Posix
    }


type alias NavigationInfo =
    { tabViewing : Tab
    }


type alias SettingsInfo =
    { identities : List Identity
    , theme : Theme
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
    , messageSubmission : MessageSubmission
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
    , tagsViewing : S.Set String
    }


type alias MailInfo =
    { activeConvo : String
    , conversations : D.Dict String Mailbox
    }


type alias Mailbox =
    { info : Conversation
    , messages : List Message
    }


type alias Message =
    { timestamp : Int
    , text : String
    , content : Maybe Multimedia
    , id : String
    , pubKey : String
    , tripcode : Maybe String
    , sig : String
    , encPubKey : String
    , recipient : String
    }


type alias MessageSubmission =
    { timestamp : Int
    , text : String
    , content : String
    , contentKind : MultimediaKind
    , tripcode : Maybe String
    , pubKey : String
    , encPubKey : String
    , recipient : String
    }


messageFromSubmission : MessageSubmission -> Message
messageFromSubmission sub =
    { timestamp = sub.timestamp
    , text = sub.text
    , content =
        if sub.content /= "" then
            Just (Multimedia sub.content sub.contentKind)

        else
            Nothing
    , id = ""
    , pubKey = sub.pubKey
    , encPubKey = sub.encPubKey
    , tripcode = sub.tripcode
    , sig = ""
    , recipient = sub.recipient
    }


messageEncoder : Message -> JE.Value
messageEncoder m =
    [ ( "timestamp", m.timestamp ) |> Opt.field JE.int
    , ( "text", m.text ) |> Opt.field JE.string
    , ( "content", m.content |> M.map multimediaEncoder |> M.withDefault JE.null ) |> Opt.field identity
    , ( "id", m.id ) |> Opt.field JE.string
    , ( "pubKey", m.pubKey ) |> Opt.field JE.string
    , ( "sig", m.sig ) |> Opt.field JE.string
    , ( "tripcode", m.tripcode ) |> Opt.optionalField JE.string
    , ( "encPubKey", m.encPubKey ) |> Opt.field JE.string
    , ( "recipient", m.recipient ) |> Opt.field JE.string
    ]
        |> Opt.objectMaySkip


messageDecoder : JD.Decoder Message
messageDecoder =
    JD.succeed Message
        |> andMap (field "timestamp" int)
        |> andMap (field "text" string)
        |> andMap (field "content" (nullable multimediaDecoder))
        |> andMap (field "id" string)
        |> andMap (field "pubKey" string)
        |> andMap (maybe (field "tripcode" string))
        |> andMap (field "sig" string)
        |> andMap (field "encPubKey" string)
        |> andMap (field "recipient" string)


settingsDecoder : JD.Decoder SettingsInfo
settingsDecoder =
    JD.map2 SettingsInfo (JD.field "identities" (JD.list identityDecoder)) (JD.field "theme" (JD.map (flip D.get themes >> M.withDefault defaultTheme) string))


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


setTags : List String -> Submission -> Submission
setTags tags s =
    { s | tags = tags }


setSubmitting : Maybe Post -> SubmissionInfo -> SubmissionInfo
setSubmitting s m =
    { m | submitting = s }


setCommentSubmitting : Maybe Comment -> SubmissionInfo -> SubmissionInfo
setCommentSubmitting s m =
    { m | commentSubmitting = s }


setCommentSubmission : CommentSubmission -> SubmissionInfo -> SubmissionInfo
setCommentSubmission s m =
    { m | commentSubmission = s }


setMessageSubmission : MessageSubmission -> SubmissionInfo -> SubmissionInfo
setMessageSubmission s m =
    { m | messageSubmission = s }


setMessageContentKind : MultimediaKind -> MessageSubmission -> MessageSubmission
setMessageContentKind c m =
    { m | contentKind = c }


setMessageText : String -> MessageSubmission -> MessageSubmission
setMessageText t m =
    { m | text = t }


setMessageContent : String -> MessageSubmission -> MessageSubmission
setMessageContent c m =
    { m | content = c }


setMessageTripcode : Maybe String -> MessageSubmission -> MessageSubmission
setMessageTripcode c m =
    { m | tripcode = c }


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


setMailboxInfo : MailInfo -> Model -> Model
setMailboxInfo m model =
    { model | mailInfo = m }


setActiveConvo : String -> Conversation -> MailInfo -> MailInfo
setActiveConvo c convo m =
    let
        withConvo =
            if D.member c m.conversations then
                m.conversations

            else
                D.insert c { info = convo, messages = [] } m.conversations
    in
    { m | activeConvo = c, conversations = withConvo }


convoPubKey : String -> Model -> Maybe String
convoPubKey c model =
    D.get c model.mailInfo.conversations |> M.map (.info >> .encPubKey)


addMessage : String -> Message -> Model -> Model
addMessage sender m model =
    let
        convoInfo =
            { tripcode = m.tripcode, pubKey = sender, encPubKey = sender }
    in
    let
        mailbox =
            D.get (sha256 sender) model.mailInfo.conversations |> M.withDefault { info = convoInfo, messages = [] }
    in
    let
        messages =
            mailbox.messages
    in
    if messages |> L.map messageId |> L.member (messageId m) then
        model

    else
        model |> (D.insert (sha256 sender) { mailbox | messages = m :: messages } model.mailInfo.conversations |> MailInfo model.mailInfo.activeConvo |> setMailboxInfo)


messageSender : Message -> Model -> String
messageSender message model =
    if model.settingsInfo.identities |> L.map .encPubKey |> L.filterMap identity |> L.member message.encPubKey then
        message.recipient

    else
        message.encPubKey


messageDomestic : Message -> Model -> String
messageDomestic message model =
    if model.settingsInfo.identities |> L.map .encPubKey |> L.filterMap identity |> L.member message.encPubKey then
        message.encPubKey

    else
        message.recipient


mailboxForeigner : Mailbox -> Model -> Maybe String
mailboxForeigner mailbox model =
    mailbox.messages |> L.head |> M.map (flip messageSender model)


mailboxDomestic : Mailbox -> Model -> Maybe String
mailboxDomestic mailbox model =
    mailbox.messages |> L.head |> M.map (flip messageDomestic model)


messageId : Message -> String
messageId m =
    sha256 (m.text ++ String.fromInt m.timestamp)


currRecipKey : Model -> Maybe String
currRecipKey model =
    D.get model.mailInfo.activeConvo model.mailInfo.conversations |> M.map (.info >> .encPubKey)


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
