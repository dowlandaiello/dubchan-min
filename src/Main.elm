port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Captcha exposing (Captcha, captchaDecoder, hash)
import Dict as D
import Html exposing (Html, div, h1, img, input, p, text)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import List as L
import Maybe as M
import Msg exposing (Msg(..))
import Post exposing (Comment, CommentSubmission, MultimediaKind(..), Post, Submission, commentDecoder, commentEncoder, commentFromSubmission, commentId, descending, fromSubmission, getChunkTime, isValidHash, postChunkDecoder, postDecoder, postEncoder, postId, pushComment, setContent, setContentKind, setText, setTitle, submissionFromComment, submissionFromPost, viewCommentArea, viewCommentText, viewMultimedia, viewPost, viewSubmitPost, viewTimestamp)
import Route exposing (..)
import Set as S
import String
import Time
import Url
import Url.Parser exposing (parse, query)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , feed : D.Dict String Post
    , feedDisplay : D.Dict Int (List Post)
    , comments : D.Dict String (D.Dict String Comment)
    , viewing : Maybe Post
    , hidden : S.Set String
    , submission : Submission
    , commentSubmission : CommentSubmission
    , time : Time.Posix
    , searchQuery : String
    , visibleMedia : S.Set String
    , blurImages : Bool
    , lastChunk : Int
    , head : Maybe Post
    , captchaHead : Captcha
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
    [ "atZytiL2hoFVzhtsPAcM9q57iGdHNFF0dbk6VQf+TqM=", "8tAA5rNHiaWrOs+rIRhJLOjqgNb2qxIT1AuMASg+1Rg=", "LJrSVEm9XaAO7Y8hN4PvHOLYGnC5ZIaJamsON4vx5YY=", "hQt7sBdkuJMRI0VJ7pBW2m01jeaAGVs3JBDLPZ9QLaY=" ]


setSubmission : Submission -> Model -> Model
setSubmission s m =
    { m | submission = s }


setCommentSubmission : CommentSubmission -> Model -> Model
setCommentSubmission s m =
    { m | commentSubmission = s }


setViewing : Post -> Model -> Model
setViewing p m =
    { m | viewing = Just p }


addComment : Comment -> Model -> Model
addComment c m =
    if c.text == "" then
        m

    else
        { m
            | comments =
                D.update c.parent
                    (\maybeComments ->
                        case maybeComments of
                            Just comments ->
                                Just (D.insert c.id c comments)

                            Nothing ->
                                Just (D.fromList [ ( c.id, c ) ])
                    )
                    m.comments
        }


addPost : Int -> Post -> Model -> Model
addPost chunk p m =
    let
        _ =
            Debug.log "" (p.id ++ " " ++ p.title ++ " " ++ String.fromInt p.timestamp)
    in
    if D.member p.id m.feed then
        m

    else
        let
            display =
                m.feedDisplay
        in
        { m
            | feed = D.insert p.id p m.feed
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
            , head =
                case m.head of
                    Just head ->
                        if p.timestamp > head.timestamp then
                            Just p

                        else
                            Just head

                    Nothing ->
                        Just p
        }


toggleHidden : String -> Model -> Model
toggleHidden parent m =
    let
        hidden =
            m.hidden
    in
    if S.member parent m.hidden then
        { m | hidden = S.remove parent m.hidden }

    else
        { m | hidden = S.insert parent m.hidden }


commentsFor : String -> Model -> Maybe (List Comment)
commentsFor s model =
    D.get s model.comments |> M.map D.values |> M.map (L.sortWith descending)


allCommentsFor : String -> Model -> Maybe (List Comment)
allCommentsFor s model =
    let
        roots =
            D.get s model.comments
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
        (D.toList model.feedDisplay
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
                            String.toLower model.searchQuery
                    in
                    String.contains q (String.toLower post.title) || String.contains q (String.toLower post.text)
                )
            |> L.map (\post -> viewPost (not (S.member post.id model.visibleMedia) && model.blurImages) (allCommentsFor post.id model |> M.map L.length |> M.withDefault 0) (L.member post.id verifiedPosts) post)
        )


viewPostComments : Model -> Html Msg
viewPostComments model =
    let
        comments =
            M.withDefault [] (M.andThen .comments model.viewing)
    in
    let
        newestComment =
            model.viewing |> M.map .id |> M.andThen (\id -> allCommentsFor id model) |> M.map (L.sortWith descending) |> M.withDefault [] |> L.head |> M.map .id |> M.withDefault ""
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
            model.commentSubmission.parent == comment.id
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
                    viewCommentArea model.commentSubmission

                  else
                    text ""
                ]
            , if L.length children > 0 then
                div [ class "subcommentsArea" ]
                    [ div [ class "commentMarker", onClick (ToggleHideChain comment.id) ] []
                    , if S.member comment.id model.hidden then
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
    update (SelectPost normalized) (Model key url (D.fromList []) (D.fromList []) (D.fromList []) Nothing S.empty (Submission "" "" "" 0 Image) (CommentSubmission "" "" "" Image 0) (Time.millisToPosix 0) "" S.empty True 0 Nothing { answer = "", data = "" })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            let
                unix =
                    Time.posixToMillis t // 1000
            in
            if getChunkTime unix - model.lastChunk > 86400 then
                ( { model | time = t, lastChunk = getChunkTime unix }, loadChunk (getChunkTime unix) )

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
                    ( { model | commentSubmission = { text = "", parent = post, content = "", contentKind = Image, nonce = 0 } }, Cmd.batch [ loadPost post, getComments post ] )

                Nothing ->
                    ( { model | viewing = Nothing }, Cmd.none )

        PostLoaded p ->
            case JD.decodeValue postDecoder p of
                Ok post ->
                    let
                        postWithC =
                            { post | comments = D.get post.id model.comments |> M.map D.values }
                    in
                    ( { model | viewing = Just postWithC }, Cmd.none )

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
            case model.head of
                Just head ->
                    ( model |> setSubmission (Submission "" "" "" 0 Image), Cmd.batch [ model.submission |> fromSubmission model.captchaHead head.id (Time.posixToMillis model.time // 1000 |> epochs) model.time |> postEncoder |> submitPost, genCaptcha () ] )

                Nothing ->
                    ( model, Cmd.none )

        ChangeSubCommentText s ->
            let
                sub =
                    model.commentSubmission
            in
            ( model |> setCommentSubmission { sub | text = s }, Cmd.none )

        SubmitComment ->
            case model.viewing of
                Just viewing ->
                    let
                        vJson =
                            postEncoder viewing
                    in
                    ( model |> setCommentSubmission (CommentSubmission "" "" "" Image 0), Cmd.batch [ model.commentSubmission |> commentFromSubmission (epochsComments (Time.posixToMillis model.time // 1000)) model.time |> commentEncoder |> (\cJson -> JE.list identity [ cJson, vJson ]) |> submitComment, genCaptcha () ] )

                Nothing ->
                    ( model |> setCommentSubmission (CommentSubmission "" "" "" Image 0), Cmd.none )

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
                    case modelWithC.viewing of
                        Just viewing ->
                            ( modelWithC |> setViewing { viewing | comments = modelWithC |> commentsFor viewing.id }, getComments comment.id )

                        Nothing ->
                            ( modelWithC, getComments comment.id )

                otherwise ->
                    ( model, Cmd.none )

        ChangeSubParent id ->
            let
                sub =
                    model.commentSubmission
            in
            ( model |> setCommentSubmission { sub | parent = id }, Cmd.none )

        ClearSub ->
            ( model |> setCommentSubmission { text = "", parent = "", content = "", contentKind = Image, nonce = 0 }, Cmd.none )

        ToggleHideChain parent ->
            ( model |> toggleHidden parent, Cmd.none )

        ChangeSearchQuery q ->
            ( { model | searchQuery = q }, Cmd.none )

        ChangeSubCommentContent s ->
            let
                sub =
                    model.commentSubmission
            in
            ( model |> setCommentSubmission { sub | content = s }, Cmd.none )

        SetSubCommentContentImage ->
            let
                sub =
                    model.commentSubmission
            in
            ( model |> setCommentSubmission { sub | contentKind = Image }, Cmd.none )

        SetSubCommentContentVideo ->
            let
                sub =
                    model.commentSubmission
            in
            ( model |> setCommentSubmission { sub | contentKind = Video }, Cmd.none )

        SetMediaVisible visible media ->
            let
                vis =
                    model.visibleMedia
            in
            ( { model
                | visibleMedia =
                    if not visible then
                        S.insert media vis

                    else
                        S.remove media vis
              }
            , Cmd.none
            )

        ToggleBlurImages ->
            let
                blurred =
                    model.blurImages
            in
            ( { model | blurImages = not blurred }, Cmd.none )

        CopyString s ->
            ( model, copy s )

        ScrolledBottom ->
            let
                newChunk =
                    model.lastChunk - 86400
            in
            ( { model | lastChunk = newChunk }, loadChunk newChunk )

        GotCaptcha captchaJson ->
            case JD.decodeValue captchaDecoder captchaJson of
                Ok captcha ->
                    ( { model | captchaHead = captcha |> hash }, Cmd.none )

                Err _ ->
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


view : Model -> Browser.Document Msg
view model =
    { title = "DubChan"
    , body =
        let
            home =
                [ div
                    (case model.viewing of
                        Just _ ->
                            [ class "feedContainer", class "hidden" ]

                        Nothing ->
                            [ class "feedContainer" ]
                    )
                    [ div
                        (case model.viewing of
                            Just _ ->
                                [ class "feed", class "hidden" ]

                            Nothing ->
                                [ class "feed" ]
                        )
                        [ div [ class "logo" ] [ img [ src "/logo.png" ] [], div [ class "logoText" ] [ h1 [] [ text "DubChan" ], p [] [ text "Anonymous. Unmoderated." ] ] ]
                        , viewQuickLinks
                        , viewSubmitPost model.submission
                        , div [ class "feedControls" ]
                            [ viewSearch model.searchQuery
                            , p
                                [ onClick ToggleBlurImages
                                , if model.blurImages then
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
        case model.viewing of
            Just viewing ->
                div [ class "viewer" ]
                    [ div [ class "viewerBody" ]
                        [ div [ class "navigation" ] [ img [ src "/back.svg", onClick (SelectPost Nothing) ] [] ]
                        , viewPost False 0 (L.member viewing.id verifiedPosts) viewing
                        , if model.commentSubmission.parent /= viewing.id && model.commentSubmission.parent /= "" then
                            text ""

                          else
                            viewCommentArea model.commentSubmission
                        , viewPostComments model
                        ]
                    ]
                    :: home

            Nothing ->
                home
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ postLoaded PostLoaded, postIn PostAdded, commentIn CommentAdded, scrolledBottom (always ScrolledBottom), gotCaptcha GotCaptcha, Time.every 1 Tick ]


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
