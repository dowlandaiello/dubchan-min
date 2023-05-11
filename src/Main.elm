port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Captcha exposing (Captcha, captchaDecoder, hash)
import Dict as D
import Html exposing (Html, canvas, div, h1, img, input, p, text)
import Html.Attributes exposing (class, id, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import List as L
import Maybe as M
import Msg exposing (Msg(..))
import Post exposing (Comment, CommentSubmission, MultimediaKind(..), Post, Submission, commentDecoder, commentEncoder, commentFromSubmission, commentId, descending, fromSubmission, getChunkTime, isValidHash, postChunkDecoder, postDecoder, postEncoder, postId, pushComment, setContent, setContentKind, setText, setTitle, subContentValid, submissionFromComment, submissionFromPost, viewCommentArea, viewCommentText, viewMultimedia, viewPost, viewSubmitPost, viewTimestamp)
import Random
import Route exposing (..)
import Set as S
import String
import Time
import Url
import Url.Parser exposing (parse, query)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , subInfo : SubmissionInfo
    , feedInfo : FeedInfo
    , time : Time.Posix
    }


type alias SubmissionInfo =
    { submission : Submission
    , commentSubmission : CommentSubmission
    , submitting : Maybe Post
    , submissionFeedback : String
    , head : Maybe Post
    , captchaHead : Captcha
    }


type alias FeedInfo =
    { searchQuery : String
    , visibleMedia : S.Set String
    , blurImages : Bool
    , lastChunk : Int
    , qotd : String
    , feed : D.Dict String Post
    , feedDisplay : D.Dict Int (List Post)
    , comments : D.Dict String (D.Dict String Comment)
    , captchas : D.Dict String Captcha
    , viewing : Maybe Post
    , hidden : S.Set String
    }


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
    [ "atZytiL2hoFVzhtsPAcM9q57iGdHNFF0dbk6VQf+TqM=", "8tAA5rNHiaWrOs+rIRhJLOjqgNb2qxIT1AuMASg+1Rg=", "LJrSVEm9XaAO7Y8hN4PvHOLYGnC5ZIaJamsON4vx5YY=", "hQt7sBdkuJMRI0VJ7pBW2m01jeaAGVs3JBDLPZ9QLaY=", "Xb0QMuARwBEIbP+DUyeywdtHKIzKb9ax2VajN47gjUs=", "bAYLk1YbUHySKEB5VU3KgEq9DzxvAR3V8c2Fg+k+zFo=", "LMx3pwHFkIWbYwVVlEHdD3B1kK0Tr+ZLN4biC6GB8uA=", "9IORdvUHPFO1p/S9YsgvElTg3sA0rHTNfNIWYIV3bBc=" ]


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
    ]


setSubmissionInfo : SubmissionInfo -> Model -> Model
setSubmissionInfo s m =
    { m | subInfo = s }


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


setLastChunk : Int -> FeedInfo -> FeedInfo
setLastChunk c m =
    { m | lastChunk = c }


setSubmission : Submission -> SubmissionInfo -> SubmissionInfo
setSubmission s m =
    { m | submission = s }


setSubmitting : Maybe Post -> SubmissionInfo -> SubmissionInfo
setSubmitting s m =
    { m | submitting = s }


setCommentSubmission : CommentSubmission -> SubmissionInfo -> SubmissionInfo
setCommentSubmission s m =
    { m | commentSubmission = s }


setSubmissionFeedback : String -> SubmissionInfo -> SubmissionInfo
setSubmissionFeedback s m =
    { m | submissionFeedback = s }


setSearchQuery : String -> FeedInfo -> FeedInfo
setSearchQuery s m =
    { m | searchQuery = s }


setViewing : Maybe Post -> FeedInfo -> FeedInfo
setViewing p m =
    { m | viewing = p }


setHead : Maybe Post -> SubmissionInfo -> SubmissionInfo
setHead p m =
    { m | head = p }


setHidden : S.Set String -> FeedInfo -> FeedInfo
setHidden h m =
    { m | hidden = h }


setComments : D.Dict String (D.Dict String Comment) -> FeedInfo -> FeedInfo
setComments c m =
    { m | comments = c }


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
    if D.member p.id m.feedInfo.feed then
        m

    else
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


viewPosts : Model -> Html Msg
viewPosts model =
    div []
        (D.toList model.feedInfo.feedDisplay
            |> L.sortBy Tuple.first
            |> L.reverse
            |> L.map Tuple.second
            |> L.concatMap (List.sortWith (sortActivity model))
            |> L.filter (\post -> not (String.isEmpty (String.filter ((/=) ' ') post.title)))
            |> L.filter (\post -> not (L.member post.id susPosts) && not (String.startsWith "fuck you" post.title))
            |> L.filter (\post -> isValidHash (epochs post.timestamp) post.hash)
            |> L.filter
                (\post ->
                    let
                        q =
                            String.toLower model.feedInfo.searchQuery
                    in
                    String.contains q (String.toLower post.title) || String.contains q (String.toLower post.text)
                )
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
        commentContent =
            [ div
                (if highlighted then
                    [ class "commentBody", class "highlighted" ]

                 else
                    [ class "commentBody" ]
                )
                [ div [ class "commentActions" ]
                    [ p [ class "commentTimestamp" ] [ viewTimestamp comment.timestamp ]
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
                    ]
                , div [ class "commentContent" ] [ viewMultimedia comment.content, viewCommentText comment.text ]
                , if replying then
                    viewCommentArea model.subInfo.submissionFeedback model.subInfo.commentSubmission

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
        [ p [ onClick (SelectPost (Just "p207+dcU6eJOzXyIVa6BxJDvBA0unmUXYweQny1SEzI=")) ] [ text "About" ], p [ onClick (SelectPost (Just "yhKvB2keb6X1U+IeU/LAhppLUCIXRyaDLxkek0T4Ag4=")) ] [ text "Contact" ], p [ onClick (SelectPost (Just "xqtklwedVIZKEL8MpZgWg2ktIPp8FE1FIvCbvG51r04=")) ] [ text "Donations" ], p [ onClick (SelectPost (Just "eAU+0RjyMATQsnI/l8HYl9EYy2Y6dtcokHkZ5UWr8VI=")) ] [ text "Discord" ] ]


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
            Model key url (SubmissionInfo (Submission "" "" "" 0 Image) (CommentSubmission "" "" "" Image 0) Nothing "" Nothing (Captcha "" "")) (FeedInfo "" S.empty True 0 "" (D.fromList []) (D.fromList []) (D.fromList []) (D.fromList []) Nothing S.empty) (Time.millisToPosix 0)
    in
    case normalized of
        Just post ->
            ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { text = "", parent = post, content = "", contentKind = Image, nonce = 0 }), Cmd.batch ([ loadPost post, getComments post ] ++ loadCmd) )

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
                ( { model | time = t } |> setFeedInfo (model.feedInfo |> setLastChunk (getChunkTime unix)), loadChunk (getChunkTime unix) )

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
                    ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { text = "", parent = post, content = "", contentKind = Image, nonce = 0 }), Cmd.batch [ loadPost post, getComments post ] )

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
                    ( model |> addPost chunk.timestamp post, getComments post.id )

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
                            ( model |> setSubmissionInfo (model.subInfo |> setSubmission (Submission "" "" "" 0 Image) |> setSubmitting Nothing |> setSubmissionFeedback ""), Cmd.batch [ submitting |> postEncoder |> submitPost, genCaptcha () ] )

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
                    ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission (CommentSubmission "" "" "" Image 0) |> setSubmissionFeedback ""), Cmd.batch [ model.subInfo.commentSubmission |> commentFromSubmission (epochsComments (Time.posixToMillis model.time // 1000)) model.time |> commentEncoder |> (\cJson -> JE.list identity [ cJson, vJson ]) |> submitComment, genCaptcha () ] )

                Nothing ->
                    ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission (CommentSubmission "" "" "" Image 0) |> setSubmissionFeedback ""), Cmd.none )

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
            ( model |> setSubmissionInfo (model.subInfo |> setCommentSubmission { text = "", parent = "", content = "", contentKind = Image, nonce = 0 } |> setSubmitting Nothing |> setSubmissionFeedback ""), Cmd.none )

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
                    model.feedInfo.lastChunk - 86400
            in
            ( model |> setFeedInfo (model.feedInfo |> setLastChunk newChunk), loadChunk newChunk )

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
                    update SubmitComment model

                Err e ->
                    ( model |> setSubmissionInfo (model.subInfo |> setSubmissionFeedback e), Cmd.none )

        RefreshPostCaptcha ->
            case model.subInfo.submitting of
                Just submitting ->
                    ( model, loadCaptcha (submitting |> postEncoder) )

                Nothing ->
                    ( model, Cmd.none )

        LoadedCaptcha captchaJson ->
            case JD.decodeValue captchaDecoder captchaJson of
                Ok captcha ->
                    case model.subInfo.submitting of
                        Just submitting ->
                            ( model |> setFeedInfo (model.feedInfo |> setCaptcha submitting.id captcha), Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ChangeSubCaptchaAnswer answer ->
            ( model
                |> setSubmissionInfo
                    (model.subInfo
                        |> setSubmitting
                            (case model.subInfo.submitting of
                                Just submitting ->
                                    Just (submitting |> setCaptchaAnswer answer)

                                Nothing ->
                                    model.subInfo.submitting
                            )
                    )
            , Cmd.none
            )


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


view : Model -> Browser.Document Msg
view model =
    { title = "DubChan"
    , body =
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
                        [ div [ class "logo" ] [ img [ src "/logo.png" ] [], div [ class "logoText" ] [ h1 [] [ text "DubChan" ], p [] [ text "Anonymous. Unmoderated." ] ] ]
                        , viewQuickLinks
                        , viewQotd model
                        , viewSubmitPost (model.subInfo.submitting |> M.map .id |> M.andThen (\id -> D.get id model.feedInfo.captchas |> M.map .data) |> M.withDefault "") (model.subInfo.submitting |> M.andThen .captchaAnswer |> M.withDefault "") model.subInfo.submissionFeedback model.subInfo.submission
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
        case model.feedInfo.viewing of
            Just viewing ->
                div [ class "viewer" ]
                    [ div [ class "viewerBody" ]
                        [ div [ class "navigation" ] [ img [ src "/back.svg", onClick (SelectPost Nothing) ] [] ]
                        , viewPost False 0 (L.member viewing.id verifiedPosts) viewing
                        , if model.subInfo.commentSubmission.parent /= viewing.id && model.subInfo.commentSubmission.parent /= "" then
                            text ""

                          else
                            viewCommentArea model.subInfo.submissionFeedback model.subInfo.commentSubmission
                        , viewPostComments model
                        ]
                    ]
                    :: home

            Nothing ->
                home
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ postLoaded PostLoaded, postIn PostAdded, commentIn CommentAdded, scrolledBottom (always ScrolledBottom), gotCaptcha GotCaptcha, loadedCaptcha LoadedCaptcha, Time.every 1 Tick ]


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
