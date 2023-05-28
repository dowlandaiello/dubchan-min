port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Captcha exposing (Captcha, captchaDecoder, captchaMsgDecoder, hash, isValidCaptcha)
import Dict as D
import Flip exposing (flip)
import Html exposing (Html, canvas, div, h1, img, input, p, text)
import Html.Attributes exposing (class, id, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Identity exposing (EncryptionRequest, Identity, SignatureRequest, encryptionRequestEncoder, identityFullname, identityShortcode, signatureRequestEncoder)
import Json.Decode as JD
import Json.Encode as JE
import List as L
import List.Extra as LE
import Mail exposing (..)
import Maybe as M
import Model exposing (..)
import Msg exposing (Conversation, Msg(..), Tab(..))
import Nav exposing (viewNavigator)
import Post exposing (Comment, CommentSubmission, MultimediaKind(..), Post, Submission, commentDecoder, commentEncoder, commentFromSubmission, commentId, descending, fromSubmission, getChunkTime, isValidHash, postChunkDecoder, postDecoder, postEncoder, postId, pushComment, setCommentSubTripcode, setContent, setContentKind, setText, setTitle, setTripcode, subContentValid, submissionFromComment, submissionFromPost, viewCommentArea, viewCommentText, viewExpandableMultimedia, viewMultimedia, viewPost, viewSubmitPost, viewTimestamp)
import Random
import Route exposing (..)
import Set as S
import Settings exposing (viewSettings)
import Sha256 exposing (sha256)
import String
import Time
import Url
import Url.Parser exposing (parse, query)


epochs : Int -> Int
epochs timestamp =
    if timestamp < 1683261744 then
        0

    else
        3


epochsComments : Int -> Int
epochsComments timestamp =
    if timestamp < 1683262043 then
        0

    else
        3


verifiedPosts =
    [ "p207+dcU6eJOzXyIVa6BxJDvBA0unmUXYweQny1SEzI=", "xqtklwedVIZKEL8MpZgWg2ktIPp8FE1FIvCbvG51r04=", "a85JYhmN0WeEP3bDN0JyF6KaNtSu7EjTE4+5pSTGrm4=", "yhKvB2keb6X1U+IeU/LAhppLUCIXRyaDLxkek0T4Ag4=", "eAU+0RjyMATQsnI/l8HYl9EYy2Y6dtcokHkZ5UWr8VI=" ]


susPosts =
    [ "atZytiL2hoFVzhtsPAcM9q57iGdHNFF0dbk6VQf+TqM=", "8tAA5rNHiaWrOs+rIRhJLOjqgNb2qxIT1AuMASg+1Rg=", "LJrSVEm9XaAO7Y8hN4PvHOLYGnC5ZIaJamsON4vx5YY=", "hQt7sBdkuJMRI0VJ7pBW2m01jeaAGVs3JBDLPZ9QLaY=", "Xb0QMuARwBEIbP+DUyeywdtHKIzKb9ax2VajN47gjUs=", "bAYLk1YbUHySKEB5VU3KgEq9DzxvAR3V8c2Fg+k+zFo=", "LMx3pwHFkIWbYwVVlEHdD3B1kK0Tr+ZLN4biC6GB8uA=", "9IORdvUHPFO1p/S9YsgvElTg3sA0rHTNfNIWYIV3bBc=", "Cy082TCxZ9+5J1ZCFg7QfOpG0o4l82Ko8K8wfNcawdI=", "gRHpIQt5DulbmY/MYxVCZvTW1sPg+ESvqwguZXpKG4c=", "QcBO0k3pPHVcJD+TZhYMooO5yAEjltkMLDByxXI6ZQA=", "24/HhaxiksspsAbqKlrGvXmZZ0rCWYBVZKtKzn5dEAg=", "1MIngQvZXVkubu35Ivks6ItHhmNAz2+SmIoTgBuv8pw=", "+Jwv4Jdbml7PJQ2/O4UjaPx0Kj2Wa83n1QQyyLrKZM0=", "QaY66zFKjqT90IjWJswmyCY9qoTEvB2n5hUfBK54IKI=" ]


quotes =
    [ "This isn't a dick pic. This is a cloudy image of someone holding a hamburger in the shower"
    , "If you haven't seen my dick you need to lurk more"
    , "this is offensive to women"
    , "Shat myself during a party 😳😳"
    , "I will unironically go I want to meet the shower pics guy"
    , "fuck you128"
    , "Appreciate the service but developer would probably be better off using php."
    , "i have no friends and now i'm also an alcoholic"
    , "The only good transphobic fascist is a dead one!"
    , "Who wants to see me cum"
    , "I have a headache every time I think about cooking. Any trad wife candidate looking for a husband? I'll earn a lot of money."
    , "bro literally went on a date with a catboy this is not earth shattering news to me"
    , "God gave me a girldick and God gave me lust. Not my fault."
    , "Cool story bro do you know how I can get a pet puppygirl"
    ]


viewPosts : Model -> Html Msg
viewPosts model =
    div []
        (D.toList model.feedInfo.feedDisplay
            |> L.sortBy Tuple.first
            |> L.reverse
            |> L.map Tuple.second
            |> L.concatMap (List.sortWith (sortActivity model))
            |> L.filter (\post -> not (String.isEmpty (String.filter ((/=) ' ') post.title)))
            |> L.filter (\post -> not (L.member post.id susPosts) && not (String.startsWith "fuck you" post.title) && not (String.startsWith "This is why you need a CAPTCHA." post.title))
            |> L.filter (\post -> model.feedInfo.captchas |> D.get post.hash |> M.map (isValidCaptcha (post.captchaAnswer |> M.withDefault "")) |> M.withDefault True)
            |> L.filter (\post -> isValidHash (epochs post.timestamp) post.hash)
            |> L.filter
                (\post ->
                    let
                        q =
                            String.toLower model.feedInfo.searchQuery
                    in
                    String.contains q (String.toLower post.title) || String.contains q (String.toLower post.text)
                )
            |> LE.uniqueBy .id
            |> L.map (\post -> viewPost (not (S.member post.id model.feedInfo.visibleMedia) && model.feedInfo.blurImages) (allCommentsFor post.id model |> M.map L.length |> M.withDefault 0) (L.member post.id verifiedPosts) post)
        )


viewPostComments : Model -> Html Msg
viewPostComments model =
    let
        comments =
            M.withDefault [] (M.andThen .comments model.feedInfo.viewing)
    in
    let
        newestComment =
            model.feedInfo.viewing |> M.map .id |> M.andThen (\id -> allCommentsFor id model) |> M.map (L.sortWith descending) |> M.withDefault [] |> L.head |> M.map .id |> M.withDefault ""
    in
    div [ class "comments" ] (comments |> L.sortWith descending |> L.filter (\comment -> comment.text /= "") |> L.map (\comment -> viewComment newestComment model comment))


viewComment : String -> Model -> Comment -> Html Msg
viewComment highlightedComment model comment =
    let
        highlighted =
            highlightedComment == comment.id
    in
    let
        replying =
            model.subInfo.commentSubmission.parent == comment.id
    in
    let
        children =
            M.withDefault [] (commentsFor comment.id model) |> L.map (viewComment highlightedComment model)
    in
    let
        expanded =
            model.feedInfo.expandedMedia |> S.member comment.id
    in
    let
        commentContent =
            [ div
                (if highlighted then
                    [ class "commentBody", class "highlighted" ]

                 else
                    [ class "commentBody" ]
                )
                [ div [ class "commentActions" ]
                    [ p [ class "commentTimestamp" ] [ viewTimestamp comment.timestamp ]
                    , case comment.pubKey of
                        Just pubKey ->
                            p [ class "commentAuthor" ] [ text (identityFullname comment.tripcode pubKey) ]

                        Nothing ->
                            text ""
                    , img
                        [ src "/reply.svg"
                        , onClick
                            (if replying then
                                ChangeSubParent comment.parent

                             else
                                ChangeSubParent comment.id
                            )
                        ]
                        []
                    , case comment.pubKey of
                        Just pubKey ->
                            comment.encPubKey |> M.map (Conversation comment.tripcode pubKey >> OpenConvo >> onClick >> L.singleton >> (++) [ class "dmButton", src "/message.svg" ] >> flip img []) |> M.withDefault (text "")

                        Nothing ->
                            text ""
                    ]
                , div
                    (if expanded then
                        [ class "commentContent", class "expanded" ]

                     else
                        [ class "commentContent" ]
                    )
                    [ viewExpandableMultimedia expanded comment.content comment.id, viewCommentText comment.text ]
                , if replying then
                    viewCommentArea model.subInfo.subIdentity model.settingsInfo.identities (model.subInfo.commentSubmitting |> M.map .hash |> M.andThen (\id -> D.get id model.feedInfo.captchas |> M.map .data) |> M.withDefault "") (model.subInfo.commentSubmitting |> M.andThen .captchaAnswer |> M.withDefault "") model.subInfo.submissionFeedback model.subInfo.commentSubmission

                  else
                    text ""
                ]
            , if L.length children > 0 then
                div [ class "subcommentsArea" ]
                    [ div [ class "commentMarker", onClick (ToggleHideChain comment.id) ] []
                    , if S.member comment.id model.feedInfo.hidden then
                        text ""

                      else
                        div [ class "subcomments" ] children
                    ]

              else
                text ""
            ]
    in
    div
        (if highlighted then
            [ class "comment", class "highlighted" ]

         else
            [ class "comment" ]
        )
        commentContent


viewSearch : String -> Html Msg
viewSearch query =
    div [ class "searchArea" ]
        [ img [ src "/search.svg" ] []
        , input [ placeholder "Search for something", value query, onInput ChangeSearchQuery ] []
        ]


normalizePostId : String -> String
normalizePostId id =
    if String.endsWith "=" id then
        id

    else
        id ++ "="


viewQotd : Model -> Html Msg
viewQotd m =
    p [ class "qotd" ] [ text m.feedInfo.qotd ]


viewQuickLinks : Html Msg
viewQuickLinks =
    div [ class "linksArea" ]
        [ p [ onClick (SelectPost (Just "p207+dcU6eJOzXyIVa6BxJDvBA0unmUXYweQny1SEzI=")) ] [ text "About" ], p [ onClick (SelectPost (Just "yhKvB2keb6X1U+IeU/LAhppLUCIXRyaDLxkek0T4Ag4=")) ] [ text "Contact" ], p [ onClick (SelectPost (Just "eAU+0RjyMATQsnI/l8HYl9EYy2Y6dtcokHkZ5UWr8VI=")) ] [ text "Discord" ] ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        p =
            url |> parse (query routeParser) |> Maybe.withDefault Nothing
    in
    let
        normalized =
            p
                |> M.map
                    normalizePostId
    in
    let
        loadCmd =
            [ Random.generate NewQuote (Random.int 0 (L.length quotes) |> Random.map (\i -> L.take i quotes |> L.reverse |> L.head |> M.withDefault "")) ]
    in
    let
        model =
            Model key url (SubmissionInfo (Submission "" "" "" 0 Image Nothing Nothing Nothing) (CommentSubmission "" "" "" Image 0 Nothing Nothing Nothing) Nothing Nothing "" Nothing (Captcha "" "") Nothing (MessageSubmission 0 "" "" Image Nothing "" "" "")) (FeedInfo "" S.empty S.empty True 0 0 "" (D.fromList []) (D.fromList []) (D.fromList []) (D.fromList []) Nothing S.empty) (NavigationInfo Feed) (SettingsInfo []) (MailInfo "" (D.fromList [])) (Time.millisToPosix 0)
    in
    case normalized of
        Just post ->
            ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { text = "", parent = post, content = "", contentKind = Image, nonce = 0, tripcode = Nothing, pubKey = Nothing, encPubKey = Nothing }), Cmd.batch ([ loadPost post, getComments post ] ++ loadCmd) )

        Nothing ->
            ( model |> setFeedInfo (model.feedInfo |> setViewing Nothing), Cmd.batch loadCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            let
                unix =
                    Time.posixToMillis t // 1000
            in
            if getChunkTime unix - model.feedInfo.lastChunk > 86400 then
                ( { model | time = t }
                    |> setFeedInfo
                        (model.feedInfo
                            |> setLastChunk (getChunkTime unix)
                            |> setLastChunkPaginated
                                (if model.feedInfo.lastChunkPaginated == 0 then
                                    getChunkTime unix

                                 else
                                    model.feedInfo.lastChunkPaginated
                                )
                        )
                , loadChunk (getChunkTime unix)
                )

            else
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
                        SelectPost (route |> M.map normalizePostId)
                    )
                    (url
                        |> parse (query routeParser)
                    )
            of
                Just m ->
                    update m model

                Nothing ->
                    ( model, Cmd.none )

        SelectPost p ->
            case p of
                Just post ->
                    ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { text = "", parent = post, content = "", contentKind = Image, nonce = 0, tripcode = Nothing, pubKey = Nothing, encPubKey = Nothing }), Cmd.batch [ loadPost post, getComments post ] )

                Nothing ->
                    ( model |> setFeedInfo (model.feedInfo |> setViewing Nothing), Cmd.none )

        PostLoaded p ->
            case JD.decodeValue postDecoder p of
                Ok post ->
                    let
                        postWithC =
                            { post | comments = D.get post.id model.feedInfo.comments |> M.map D.values }
                    in
                    ( model |> setFeedInfo (model.feedInfo |> setViewing (Just postWithC)), Cmd.none )

                otherwise ->
                    ( model, Cmd.none )

        PostAdded p ->
            case JD.decodeValue postChunkDecoder p of
                Ok chunk ->
                    let
                        post =
                            chunk.post
                    in
                    let
                        hashed =
                            { post | hash = postId (Time.millisToPosix (post.timestamp * 1000)) (submissionFromPost post) }
                    in
                    ( model |> addPost chunk.timestamp post
                    , if not (D.member post.hash model.feedInfo.captchas) then
                        getComments post.id

                      else
                        Cmd.batch [ getComments post.id, loadCaptcha (hashed |> postEncoder) ]
                    )

                otherwise ->
                    ( model, Cmd.none )

        ChangeSubTitle s ->
            ( model |> setSubmissionInfo (model.subInfo |> setSubmission (model.subInfo.submission |> setTitle s)), Cmd.none )

        ChangeSubText s ->
            ( model |> setSubmissionInfo (model.subInfo |> setSubmission (model.subInfo.submission |> setText s)), Cmd.none )

        ChangeSubContent s ->
            ( model |> setSubmissionInfo (model.subInfo |> setSubmission (model.subInfo.submission |> setContent s)), Cmd.none )

        SetSubContentImage ->
            ( model |> setSubmissionInfo (model.subInfo |> setSubmission (model.subInfo.submission |> setContentKind Image)), Cmd.none )

        SetSubContentVideo ->
            ( model |> setSubmissionInfo (model.subInfo |> setSubmission (model.subInfo.submission |> setContentKind Video)), Cmd.none )

        SubmitPost ->
            case model.subInfo.head of
                Just head ->
                    case model.subInfo.submitting of
                        Just submitting ->
                            case model.feedInfo.captchas |> D.get submitting.hash of
                                Just expected ->
                                    let
                                        pubKey =
                                            model.subInfo.subIdentity |> M.map .pubKey
                                    in
                                    let
                                        encPubKey =
                                            model.subInfo.subIdentity |> M.andThen .encPubKey
                                    in
                                    let
                                        json =
                                            { submitting | pubKey = pubKey, encPubKey = encPubKey } |> postEncoder
                                    in
                                    let
                                        toSubmit =
                                            case model.subInfo.subIdentity of
                                                Just identity ->
                                                    json |> SignatureRequest identity.privKey identity.encPrivKey |> signatureRequestEncoder

                                                Nothing ->
                                                    json
                                    in
                                    if isValidCaptcha (submitting.captchaAnswer |> M.withDefault "") expected then
                                        ( model |> setSubmissionInfo (model.subInfo |> setSubmission (Submission "" "" "" 0 Image Nothing Nothing Nothing) |> setSubmitting Nothing |> setSubmissionFeedback "" |> setSubIdentity Nothing), Cmd.batch [ toSubmit |> submitPost, genCaptcha () ] )

                                    else
                                        ( model |> setSubmissionInfo (model.subInfo |> setSubmissionFeedback "Invalid captcha response."), Cmd.none )

                                Nothing ->
                                    ( model |> setSubmissionInfo (model.subInfo |> setSubmissionFeedback "No captcha available."), Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ChangeSubCommentText s ->
            let
                sub =
                    model.subInfo.commentSubmission
            in
            ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { sub | text = s }), Cmd.none )

        SubmitComment ->
            case model.feedInfo.viewing of
                Just viewing ->
                    let
                        vJson =
                            postEncoder viewing
                    in
                    case model.subInfo.commentSubmitting of
                        Just submitting ->
                            case model.feedInfo.captchas |> D.get submitting.hash of
                                Just expected ->
                                    let
                                        pubKey =
                                            model.subInfo.subIdentity |> M.map .pubKey
                                    in
                                    let
                                        encPubKey =
                                            model.subInfo.subIdentity |> M.andThen .encPubKey
                                    in
                                    let
                                        json =
                                            { submitting | pubKey = pubKey, encPubKey = encPubKey } |> commentEncoder
                                    in
                                    let
                                        toSubmit =
                                            case model.subInfo.subIdentity of
                                                Just identity ->
                                                    json |> SignatureRequest identity.privKey identity.encPrivKey |> signatureRequestEncoder

                                                Nothing ->
                                                    json
                                    in
                                    if isValidCaptcha (submitting.captchaAnswer |> M.withDefault "") expected then
                                        ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission (CommentSubmission "" "" "" Image 0 Nothing Nothing Nothing) |> setCommentSubmitting Nothing |> setSubmissionFeedback ""), Cmd.batch [ toSubmit |> (\cJson -> JE.list identity [ cJson, vJson ]) |> submitComment, genCaptcha () ] )

                                    else
                                        ( model |> setSubmissionInfo (model.subInfo |> setSubmissionFeedback "Invalid captcha response."), Cmd.none )

                                Nothing ->
                                    ( model |> setSubmissionInfo (model.subInfo |> setSubmissionFeedback "No captcha available."), Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission (CommentSubmission "" "" "" Image 0 Nothing Nothing Nothing) |> setSubmissionFeedback ""), Cmd.none )

        CommentAdded c ->
            case JD.decodeValue commentDecoder c of
                Ok comment ->
                    let
                        hashed =
                            { comment | hash = commentId (submissionFromComment comment) }
                    in
                    let
                        modelWithC =
                            model |> addComment hashed
                    in
                    case modelWithC.feedInfo.viewing of
                        Just viewing ->
                            ( modelWithC |> setFeedInfo (modelWithC.feedInfo |> setViewing (Just { viewing | comments = modelWithC |> commentsFor viewing.id })), getComments comment.id )

                        Nothing ->
                            ( modelWithC, getComments comment.id )

                otherwise ->
                    ( model, Cmd.none )

        ChangeSubParent id ->
            let
                sub =
                    model.subInfo.commentSubmission
            in
            ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { sub | parent = id }), Cmd.none )

        ClearSub ->
            ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { text = "", parent = "", content = "", contentKind = Image, nonce = 0, pubKey = Nothing, tripcode = Nothing, encPubKey = Nothing } |> setSubmitting Nothing |> setCommentSubmitting Nothing |> setSubmissionFeedback ""), Cmd.none )

        ToggleHideChain parent ->
            ( model |> toggleHidden parent, Cmd.none )

        ChangeSearchQuery q ->
            ( model |> setFeedInfo (model.feedInfo |> setSearchQuery q), Cmd.none )

        ChangeSubCommentContent s ->
            let
                sub =
                    model.subInfo.commentSubmission
            in
            ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { sub | content = s }), Cmd.none )

        SetSubCommentContentImage ->
            let
                sub =
                    model.subInfo.commentSubmission
            in
            ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { sub | contentKind = Image }), Cmd.none )

        SetSubCommentContentVideo ->
            let
                sub =
                    model.subInfo.commentSubmission
            in
            ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { sub | contentKind = Video }), Cmd.none )

        SetMediaVisible visible media ->
            let
                vis =
                    model.feedInfo.visibleMedia
            in
            ( model
                |> setFeedInfo
                    (model.feedInfo
                        |> setVisibleMedia
                            (if not visible then
                                S.insert media vis

                             else
                                S.remove media vis
                            )
                    )
            , Cmd.none
            )

        ToggleBlurImages ->
            let
                blurred =
                    model.feedInfo.blurImages
            in
            ( model |> setFeedInfo (model.feedInfo |> setBlurImages (not blurred)), Cmd.none )

        CopyString s ->
            ( model, copy s )

        ScrolledBottom ->
            let
                newChunk =
                    model.feedInfo.lastChunkPaginated - 86400
            in
            ( model |> setFeedInfo (model.feedInfo |> setLastChunkPaginated newChunk), loadChunk newChunk )

        GotCaptcha captchaJson ->
            case JD.decodeValue captchaDecoder captchaJson of
                Ok captcha ->
                    ( model |> setSubmissionInfo (model.subInfo |> setCaptchaHead (captcha |> hash)), Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        NewQuote q ->
            ( model |> setFeedInfo (model.feedInfo |> setQotd q), Cmd.none )

        ValidatePost ->
            case subContentValid model.subInfo.submission of
                Ok _ ->
                    case model.subInfo.head of
                        Just head ->
                            let
                                submitting =
                                    model.subInfo.submission |> fromSubmission model.subInfo.captchaHead head.id (Time.posixToMillis model.time // 1000 |> epochs) model.time
                            in
                            update RefreshPostCaptcha (model |> setSubmissionInfo (model.subInfo |> setSubmitting (Just submitting)))

                        Nothing ->
                            ( model |> setSubmissionInfo (model.subInfo |> setSubmissionFeedback "No previous post to attach to."), Cmd.none )

                Err e ->
                    ( model |> setSubmissionInfo (model.subInfo |> setSubmissionFeedback e), Cmd.none )

        ValidateComment ->
            case subContentValid model.subInfo.commentSubmission of
                Ok _ ->
                    case model.subInfo.head of
                        Just head ->
                            let
                                submitting =
                                    model.subInfo.commentSubmission |> commentFromSubmission (epochsComments (Time.posixToMillis model.time // 1000)) model.time
                            in
                            update RefreshCommentCaptcha (model |> setSubmissionInfo (model.subInfo |> setCommentSubmitting (Just submitting)))

                        Nothing ->
                            ( model |> setSubmissionInfo (model.subInfo |> setSubmissionFeedback "No previous post to attach to."), Cmd.none )

                Err e ->
                    ( model |> setSubmissionInfo (model.subInfo |> setSubmissionFeedback e), Cmd.none )

        RefreshPostCaptcha ->
            case model.subInfo.submitting of
                Just submitting ->
                    ( model, loadCaptcha (submitting |> postEncoder) )

                Nothing ->
                    ( model, Cmd.none )

        RegenPostCaptcha ->
            case model.subInfo.submitting of
                Just submitting ->
                    let
                        nonce =
                            submitting.nonce
                    in
                    let
                        post =
                            { submitting | nonce = nonce + 1 }
                    in
                    let
                        hashed =
                            { post | hash = postId (Time.millisToPosix (post.timestamp * 1000)) (submissionFromPost post) }
                    in
                    update RefreshPostCaptcha (model |> setSubmissionInfo (model.subInfo |> setSubmitting (Just hashed)))

                Nothing ->
                    ( model, Cmd.none )

        LoadedCaptcha captchaJson ->
            case JD.decodeValue captchaMsgDecoder captchaJson of
                Ok captchaMsg ->
                    if D.member captchaMsg.post model.feedInfo.captchas then
                        ( model, Cmd.none )

                    else
                        ( model |> setFeedInfo (model.feedInfo |> setCaptcha captchaMsg.post captchaMsg.captcha), Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ChangeSubCaptchaAnswer answer ->
            ( model
                |> setSubmissionInfo
                    (model.subInfo
                        |> setSubmitting
                            (case model.subInfo.submitting of
                                Just submitting ->
                                    Just (submitting |> setCaptchaAnswer (answer |> String.toLower))

                                Nothing ->
                                    model.subInfo.submitting
                            )
                    )
            , Cmd.none
            )

        RefreshCommentCaptcha ->
            case model.subInfo.commentSubmitting of
                Just submitting ->
                    ( model, loadCaptcha (submitting |> commentEncoder) )

                Nothing ->
                    ( model, Cmd.none )

        RegenCommentCaptcha ->
            case model.subInfo.commentSubmitting of
                Just submitting ->
                    let
                        nonce =
                            submitting.nonce
                    in
                    let
                        unhashed =
                            { submitting | nonce = nonce + 1 }
                    in
                    let
                        hashed =
                            { unhashed | hash = commentId (submissionFromComment unhashed) }
                    in
                    update RefreshCommentCaptcha (model |> setSubmissionInfo (model.subInfo |> setCommentSubmitting (Just hashed)))

                Nothing ->
                    ( model, Cmd.none )

        ChangeSubCommentCaptchaAnswer answer ->
            ( model
                |> setSubmissionInfo
                    (model.subInfo
                        |> setCommentSubmitting
                            (case model.subInfo.commentSubmitting of
                                Just submitting ->
                                    Just (submitting |> setCommentCaptchaAnswer (answer |> String.toLower))

                                Nothing ->
                                    model.subInfo.commentSubmitting
                            )
                    )
            , Cmd.none
            )

        SetSubContentValid valid ->
            ( model
                |> setSubmissionInfo
                    (model.subInfo
                        |> setSubmissionFeedback
                            (if valid then
                                ""

                             else
                                "Your link is broken. Do not post."
                            )
                    )
            , Cmd.none
            )

        SetMediaExpanded expanded id ->
            ( model
                |> setFeedInfo
                    (model.feedInfo
                        |> setExpanded
                            (if expanded then
                                S.insert id model.feedInfo.expandedMedia

                             else
                                S.remove id model.feedInfo.expandedMedia
                            )
                    )
            , Cmd.none
            )

        ChangeTabViewing tab ->
            ( model |> setNavInfo (model.navInfo |> setTabViewing tab), Cmd.none )

        GotSettings json ->
            case JD.decodeValue settingsDecoder json of
                Ok settings ->
                    ( model |> setSettingsInfo settings, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GenerateIdentity ->
            ( model, generateIdentity () )

        SaveSettings ->
            let
                enc =
                    settingsEncoder model.settingsInfo
            in
            ( model, modifiedSettings enc )

        ChangeSubIdentity maybeIdenPk ->
            let
                maybeIden =
                    maybeIdenPk |> M.andThen (\idenPk -> model.settingsInfo.identities |> L.filter (\iden -> iden.pubKey == idenPk) |> L.head)
            in
            ( model |> setSubmissionInfo (model.subInfo |> setSubIdentity maybeIden), Cmd.none )

        ChangeSubTripcode trip ->
            ( model |> setSubmissionInfo (model.subInfo |> setSubmission (model.subInfo.submission |> setTripcode trip)), Cmd.none )

        ChangeSubCommentTripcode trip ->
            ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission (model.subInfo.commentSubmission |> setCommentSubTripcode trip)), Cmd.none )

        OpenConvo convo ->
            model |> setMailboxInfo (model.mailInfo |> setActiveConvo (sha256 convo.encPubKey) convo) |> setSubmissionInfo (model.subInfo |> setSubIdentity (model.settingsInfo.identities |> L.head)) |> update (ChangeTabViewing Messages)

        SetSubMessageImage ->
            ( model |> setSubmissionInfo (model.subInfo |> setMessageSubmission (model.subInfo.messageSubmission |> setMessageContentKind Image)), Cmd.none )

        SetSubMessageVideo ->
            ( model |> setSubmissionInfo (model.subInfo |> setMessageSubmission (model.subInfo.messageSubmission |> setMessageContentKind Video)), Cmd.none )

        ChangeSubMessageText text ->
            ( model |> setSubmissionInfo (model.subInfo |> setMessageSubmission (model.subInfo.messageSubmission |> setMessageText text)), Cmd.none )

        ChangeSubMessageContent c ->
            ( model |> setSubmissionInfo (model.subInfo |> setMessageSubmission (model.subInfo.messageSubmission |> setMessageContent c)), Cmd.none )

        SubmitMessage ->
            case model.subInfo.subIdentity of
                Just identity ->
                    let
                        submitting =
                            model.subInfo.messageSubmission
                    in
                    let
                        withKeys =
                            { submitting | timestamp = Time.posixToMillis model.time // 1000, encPubKey = identity.encPubKey |> M.withDefault "", pubKey = identity.pubKey, recipient = currRecipKey model |> M.withDefault "" }
                    in
                    let
                        json =
                            messageFromSubmission withKeys |> messageEncoder
                    in
                    ( model |> setSubmissionInfo (model.subInfo |> setMessageSubmission (MessageSubmission 0 "" "" Image Nothing "" "" model.subInfo.messageSubmission.recipient)), json |> EncryptionRequest identity.privKey (identity.encPubKey |> M.withDefault "") (convoPubKey model.mailInfo.activeConvo model |> M.withDefault "") |> encryptionRequestEncoder |> submitMessage )

                Nothing ->
                    ( model, Cmd.none )

        ChangeSubMessageTripcode code ->
            ( model |> setSubmissionInfo (model.subInfo |> setMessageSubmission (model.subInfo.messageSubmission |> setMessageTripcode code)), Cmd.none )

        GotDm json ->
            case JD.decodeValue messageDecoder json of
                Ok message ->
                    ( addMessage (messageSender message model) message model, Cmd.none )

                Err e ->
                    ( model, Cmd.none )


port loadPost : String -> Cmd msg


port postLoaded : (JE.Value -> msg) -> Sub msg


port postIn : (JE.Value -> msg) -> Sub msg


port commentIn : (JE.Value -> msg) -> Sub msg


port submitPost : JE.Value -> Cmd msg


port submitComment : JE.Value -> Cmd msg


port copy : String -> Cmd msg


port getComments : String -> Cmd msg


port loadChunk : Int -> Cmd msg


port scrolledBottom : (JE.Value -> msg) -> Sub msg


port genCaptcha : () -> Cmd msg


port gotCaptcha : (JE.Value -> msg) -> Sub msg


port loadCaptcha : JE.Value -> Cmd msg


port loadedCaptcha : (JE.Value -> msg) -> Sub msg


port loadedSettings : (JE.Value -> msg) -> Sub msg


port generateIdentity : () -> Cmd msg


port modifiedSettings : JE.Value -> Cmd msg


port submitMessage : JE.Value -> Cmd msg


port messageLoaded : (JE.Value -> msg) -> Sub msg


view : Model -> Browser.Document Msg
view model =
    { title = "DubChan"
    , body =
        let
            navigator =
                viewNavigator model.navInfo.tabViewing
        in
        let
            home =
                [ div
                    (case model.feedInfo.viewing of
                        Just _ ->
                            [ class "feedContainer", class "hidden" ]

                        Nothing ->
                            [ class "feedContainer" ]
                    )
                    [ div
                        (case model.feedInfo.viewing of
                            Just _ ->
                                [ class "feed", class "hidden" ]

                            Nothing ->
                                [ class "feed" ]
                        )
                        [ div [ class "logo" ] [ img [ src "/logo.png" ] [], div [ class "logoText" ] [ div [ class "logoBigLine" ] [ h1 [] [ text "DubChan" ], p [ class "betaMarker" ] [ text "Beta" ] ], p [] [ text "Anonymous. Unmoderated." ] ] ]
                        , viewQuickLinks
                        , viewQotd model
                        , viewSubmitPost model.settingsInfo.identities model.subInfo.subIdentity (model.subInfo.submitting |> M.map .hash |> M.andThen (\id -> D.get id model.feedInfo.captchas |> M.map .data) |> M.withDefault "") (model.subInfo.submitting |> M.andThen .captchaAnswer |> M.withDefault "") model.subInfo.submissionFeedback model.subInfo.submission
                        , canvas [ id "captchaGen" ] []
                        , div [ class "feedControls" ]
                            [ viewSearch model.feedInfo.searchQuery
                            , p
                                [ onClick ToggleBlurImages
                                , if model.feedInfo.blurImages then
                                    class "active"

                                  else
                                    class ""
                                ]
                                [ text "Blur Images" ]
                            ]
                        , viewPosts
                            model
                        ]
                    ]
                ]
        in
        navigator
            :: (case model.navInfo.tabViewing of
                    Feed ->
                        case model.feedInfo.viewing of
                            Just viewing ->
                                div [ class "viewer" ]
                                    [ div [ class "viewerBody" ]
                                        [ div [ class "navigation" ] [ img [ src "/back.svg", onClick (SelectPost Nothing) ] [] ]
                                        , viewPost False 0 (L.member viewing.id verifiedPosts) viewing
                                        , if model.subInfo.commentSubmission.parent /= viewing.id && model.subInfo.commentSubmission.parent /= "" then
                                            text ""

                                          else
                                            viewCommentArea model.subInfo.subIdentity model.settingsInfo.identities (model.subInfo.commentSubmitting |> M.map .hash |> M.andThen (\id -> D.get id model.feedInfo.captchas |> M.map .data) |> M.withDefault "") (model.subInfo.commentSubmitting |> M.andThen .captchaAnswer |> M.withDefault "") model.subInfo.submissionFeedback model.subInfo.commentSubmission
                                        , viewPostComments model
                                        ]
                                    ]
                                    :: home

                            Nothing ->
                                home

                    Messages ->
                        [ viewMail model ]

                    Settings ->
                        [ viewSettings model ]
               )
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ postLoaded PostLoaded, postIn PostAdded, commentIn CommentAdded, scrolledBottom (always ScrolledBottom), gotCaptcha GotCaptcha, loadedCaptcha LoadedCaptcha, loadedSettings GotSettings, messageLoaded GotDm, Time.every 1 Tick ]


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
