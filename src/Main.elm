port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict as D
import Html exposing (Html, div, h1, img, input, p, text)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import List as L
import Maybe as M
import Msg exposing (Msg(..))
import Post exposing (Comment, CommentSubmission, MultimediaKind(..), Post, Submission, commentDecoder, commentEncoder, commentFromSubmission, commentId, descending, fromSubmission, isValidHash, postDecoder, postEncoder, postId, pushComment, setContent, setContentKind, setText, setTitle, submissionFromComment, submissionFromPost, viewCommentArea, viewCommentText, viewMultimedia, viewPost, viewSubmitPost, viewTimestamp)
import Route exposing (..)
import Set as S
import String
import Time
import Url
import Url.Parser exposing (parse)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , feed : D.Dict String Post
    , comments : D.Dict String (List Comment)
    , viewing : Maybe Post
    , hidden : S.Set String
    , submission : Submission
    , commentSubmission : CommentSubmission
    , time : Time.Posix
    , searchQuery : String
    , visibleMedia : S.Set String
    , blurImages : Bool
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
    [ "p207+dcU6eJOzXyIVa6BxJDvBA0unmUXYweQny1SEzI=", "xqtklwedVIZKEL8MpZgWg2ktIPp8FE1FIvCbvG51r04=", "a85JYhmN0WeEP3bDN0JyF6KaNtSu7EjTE4+5pSTGrm4=" ]


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
                                Just (c :: comments)

                            Nothing ->
                                Just [ c ]
                    )
                    m.comments
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
    D.get s model.comments |> M.map (L.sortWith descending)


allCommentsFor : String -> Model -> Maybe (List Comment)
allCommentsFor s model =
    let
        roots =
            D.get s model.comments
    in
    roots |> M.map (L.concatMap (\comment -> comment :: M.withDefault [] (allCommentsFor comment.id model)))


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


similarityThreshold : Float
similarityThreshold =
    0.5


similarity : String -> String -> Bool
similarity a b =
    let
        aWords =
            S.fromList (String.words a)
    in
    let
        bWords =
            S.fromList (String.words b)
    in
    toFloat
        (S.size (S.intersect aWords bWords))
        / toFloat (max (S.size aWords) (S.size bWords))
        >= similarityThreshold


similarPosts : Model -> Post -> List Post
similarPosts model p =
    D.values model.feed |> L.filter (\post -> post.id /= p.id) |> L.filter (\post -> (similarity post.text p.text && post.text /= "") || similarity post.title p.title)


uniqueFactor : Model -> Post -> Float
uniqueFactor model a =
    let
        nSimilar =
            L.length (similarPosts model a)
    in
    0.01 * (toFloat (max nSimilar 0) / toFloat (D.size model.feed + 1))


sortFactor : Model -> Post -> Float
sortFactor model a =
    let
        timeFactor =
            0.99 * (toFloat (max (youngestCommentFor a.id model) a.timestamp) / 1682664445.0)
    in
    (1 - a.uniqueFactor) + timeFactor


sortUnique model a b =
    case compare (sortFactor model a) (sortFactor model b) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


viewPosts : Model -> Html Msg
viewPosts model =
    div []
        (D.values model.feed
            |> L.sortWith (sortUnique model)
            |> L.filter (\post -> not (String.isEmpty (String.filter ((/=) ' ') post.title)))
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


viewQuickLinks : Html Msg
viewQuickLinks =
    div [ class "linksArea" ]
        [ p [ onClick (SelectPost (Just "p207+dcU6eJOzXyIVa6BxJDvBA0unmUXYweQny1SEzI=")) ] [ text "About" ], p [ onClick (SelectPost (Just "xqtklwedVIZKEL8MpZgWg2ktIPp8FE1FIvCbvG51r04=")) ] [ text "Donations" ], p [ onClick (SelectPost (Just "a85JYhmN0WeEP3bDN0JyF6KaNtSu7EjTE4+5pSTGrm4=")) ] [ text "Discord" ] ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case
        parse routeParser url
    of
        Just (Route.Post p) ->
            update (SelectPost (Just p)) (Model key url (D.fromList []) (D.fromList []) Nothing S.empty (Submission "" "" "" 0 Image) (CommentSubmission "" "" "" Image 0) (Time.millisToPosix 0) "" S.empty True)

        Nothing ->
            ( Model key url (D.fromList []) (D.fromList []) Nothing S.empty (Submission "" "" "" 0 Image) (CommentSubmission "" "" "" Image 0) (Time.millisToPosix 0) "" S.empty True, Cmd.none )


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
                    ( { model | commentSubmission = { text = "", parent = post, content = "", contentKind = Image, nonce = 0 } }, loadPost post )

                Nothing ->
                    ( { model | viewing = Nothing }, Cmd.none )

        PostLoaded p ->
            case JD.decodeValue postDecoder p of
                Ok post ->
                    let
                        postWithC =
                            { post | comments = D.get post.id model.comments }
                    in
                    ( { model | viewing = Just postWithC }, Cmd.none )

                otherwise ->
                    ( model, Cmd.none )

        PostAdded p ->
            case JD.decodeValue postDecoder p of
                Ok post ->
                    let
                        hashed =
                            { post | hash = postId (Time.millisToPosix (post.timestamp * 1000)) (submissionFromPost post), uniqueFactor = uniqueFactor model post }
                    in
                    ( { model | feed = D.insert post.id hashed model.feed }, Cmd.none )

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
            ( model |> setSubmission (Submission "" "" "" 0 Image), model.submission |> fromSubmission (Time.posixToMillis model.time // 1000 |> epochs) model.time |> postEncoder |> submitPost )

        ChangeSubCommentText s ->
            let
                sub =
                    model.commentSubmission
            in
            ( model |> setCommentSubmission { sub | text = s }, Cmd.none )

        SubmitComment ->
            case model.viewing of
                Just viewing ->
                    ( model |> setCommentSubmission (CommentSubmission "" "" "" Image 0), model.commentSubmission |> commentFromSubmission (epochsComments (Time.posixToMillis model.time // 1000)) model.time |> commentEncoder |> submitComment )

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
                            ( modelWithC |> setViewing { viewing | comments = modelWithC |> commentsFor viewing.id }, Cmd.none )

                        Nothing ->
                            ( modelWithC, Cmd.none )

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
