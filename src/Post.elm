module Post exposing (..)

import Captcha exposing (Captcha, captchaDecoder, captchaEncoder)
import Hash exposing (Hash)
import Html exposing (Html, a, div, h1, iframe, img, input, label, p, text, textarea, video)
import Html.Attributes exposing (class, controls, for, height, href, id, loop, placeholder, preload, property, src, target, title, value, width)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder, Error, field, float, int, list, map2, map3, map5, map7, map8, maybe, nullable, string)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import List as L
import Maybe as M
import Msg exposing (Msg(..))
import Sha256 exposing (sha256)
import String as S
import Time exposing (Month(..), Posix, Zone, customZone, millisToPosix, posixToMillis, toDay, toHour, toMinute, toMonth, toYear)


type alias Post =
    { timestamp : Int
    , title : String
    , text : String
    , content : Maybe Multimedia
    , comments : Maybe (List Comment)
    , nonce : Int
    , captcha : Maybe Captcha
    , captchaAnswer : Maybe String
    , id : String
    , hash : String
    , prev : Maybe String
    }


type alias Submission =
    { title : String
    , text : String
    , content : String
    , captchaAnswer : String
    , nonce : Int
    , contentKind : MultimediaKind
    }


type alias CommentSubmission =
    { text : String
    , parent : String
    , content : String
    , contentKind : MultimediaKind
    , nonce : Int
    }


type alias PostChunk =
    { timestamp : Int
    , post : Post
    }


setTitle : String -> Submission -> Submission
setTitle s submission =
    { submission | title = s }


setText : String -> Submission -> Submission
setText s submission =
    { submission | text = s }


setContent : String -> Submission -> Submission
setContent s submission =
    { submission | content = s }


setContentKind : MultimediaKind -> Submission -> Submission
setContentKind s submission =
    { submission | contentKind = s }


subContentValid : { a | content : String } -> Result String ()
subContentValid s =
    if s.content /= "" && not (S.startsWith "http://" s.content) && not (S.startsWith "https://" s.content) then
        Err "Attachment must be a link starting with http:// or https://"

    else
        Ok ()


pushComment : Comment -> Post -> Post
pushComment c p =
    case p.comments of
        Just comments ->
            if L.member c comments then
                p

            else
                { p | comments = Just (c :: comments) }

        Nothing ->
            { p | comments = Just [ c ] }


submissionFromComment : Comment -> CommentSubmission
submissionFromComment c =
    let
        content =
            M.withDefault (Multimedia "" Image) c.content
    in
    CommentSubmission c.text c.parent content.src content.kind c.nonce


submissionFromPost : Post -> Submission
submissionFromPost p =
    let
        content =
            M.withDefault (Multimedia "" Image) p.content
    in
    Submission p.title p.text content.src (M.withDefault "" p.captchaAnswer) p.nonce content.kind


fromSubmission : Captcha -> String -> Int -> Posix -> Submission -> Post
fromSubmission captcha prev target time sub =
    let
        id =
            postId time sub
    in
    if isValidHash target id then
        Post (posixToMillis time // 1000)
            sub.title
            sub.text
            (if sub.content /= "" then
                Just (Multimedia sub.content sub.contentKind)

             else
                Nothing
            )
            Nothing
            sub.nonce
            (Just captcha)
            (Just sub.captchaAnswer)
            id
            id
            (Just prev)

    else
        let
            nonce =
                sub.nonce
        in
        fromSubmission captcha prev target time { sub | nonce = nonce + 1 }


commentFromSubmission : Int -> Posix -> CommentSubmission -> Comment
commentFromSubmission target time sub =
    let
        id =
            commentId sub
    in
    if isValidHash target id then
        Comment (posixToMillis time // 1000)
            sub.text
            sub.parent
            id
            (if sub.content /= "" then
                Just (Multimedia sub.content sub.contentKind)

             else
                Nothing
            )
            0
            id

    else
        let
            nonce =
                sub.nonce
        in
        commentFromSubmission target time { sub | nonce = nonce + 1 }


type alias Comment =
    { timestamp : Int
    , text : String
    , parent : String
    , id : String
    , content : Maybe Multimedia
    , nonce : Int
    , hash : String
    }


type alias Multimedia =
    { src : String
    , kind : MultimediaKind
    }


type MultimediaKind
    = Image
    | Video


postChunkDecoder : Decoder PostChunk
postChunkDecoder =
    map2 PostChunk (field "timestamp" int) (field "post" postDecoder)


commentDecoder : Decoder Comment
commentDecoder =
    map7 Comment (field "timestamp" int) (field "text" string) (field "parent" string) (field "id" string) (field "content" (nullable multimediaDecoder)) (field "nonce" int) (field "hash" string)


multimediaDecoder : Decoder Multimedia
multimediaDecoder =
    map2 Multimedia (field "src" string) (field "kind" multimediaKindDecoder)


multimediaKindDecoder : Decoder MultimediaKind
multimediaKindDecoder =
    string |> JD.andThen parseMultimediaKind


parseMultimediaKind : String -> Decoder MultimediaKind
parseMultimediaKind s =
    case s of
        "image" ->
            JD.succeed Image

        "video" ->
            JD.succeed Video

        otherwise ->
            JD.fail ("Invalid multimedia kind: " ++ s)


postEncoder : Post -> JE.Value
postEncoder p =
    let
        req =
            [ ( "timestamp", JE.int p.timestamp ), ( "title", JE.string p.title ), ( "text", JE.string p.text ), ( "content", p.content |> M.map multimediaEncoder |> M.withDefault JE.null ), ( "comments", JE.null ), ( "nonce", JE.int p.nonce ), ( "hash", JE.string p.hash ), ( "id", JE.string p.id ) ]
    in
    case p.captcha of
        Just captcha ->
            case p.prev of
                Just prev ->
                    JE.object (( "prev", JE.string prev ) :: ( "captcha", captchaEncoder captcha ) :: req)

                Nothing ->
                    JE.object req

        Nothing ->
            JE.object req


commentEncoder : Comment -> JE.Value
commentEncoder c =
    JE.object [ ( "timestamp", JE.int c.timestamp ), ( "text", JE.string c.text ), ( "parent", JE.string c.parent ), ( "id", JE.string c.id ), ( "content", c.content |> M.map multimediaEncoder |> M.withDefault JE.null ), ( "nonce", JE.int c.nonce ), ( "hash", JE.string c.hash ) ]


postId : Posix -> Submission -> String
postId t sub =
    sha256 (sub.title ++ sub.text ++ S.fromInt sub.nonce ++ S.fromInt (posixToMillis t // 1000))


commentId : CommentSubmission -> String
commentId sub =
    sha256 (sub.text ++ sub.parent ++ S.fromInt sub.nonce)


multimediaEncoder : Multimedia -> JE.Value
multimediaEncoder m =
    JE.object
        [ ( "src", JE.string m.src )
        , ( "kind"
          , JE.string
                (case m.kind of
                    Image ->
                        "image"

                    Video ->
                        "video"
                )
          )
        ]


postDecoder : Decoder Post
postDecoder =
    JD.succeed Post
        |> andMap (field "timestamp" int)
        |> andMap (field "title" string)
        |> andMap (field "text" string)
        |> andMap (field "content" (nullable multimediaDecoder))
        |> andMap (field "comments" (nullable (list commentDecoder)))
        |> andMap (field "nonce" int)
        |> andMap (maybe (field "captcha" captchaDecoder))
        |> andMap (maybe (field "captchaAnswer" string))
        |> andMap (field "id" string)
        |> andMap (field "hash" string)
        |> andMap (maybe (field "prev" string))


showMonth : Month -> String
showMonth m =
    case m of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "July"

        Aug ->
            "Aug"

        Sep ->
            "Sept"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


showTime : Posix -> String
showTime t =
    let
        z =
            customZone (-7 * 60) []
    in
    let
        suffix =
            if toHour z t >= 12 then
                "PM"

            else
                "AM"
    in
    let
        secs =
            toMinute z t
    in
    let
        secsVis =
            secs |> S.fromInt
    in
    let
        hOffset =
            modBy 12 (toHour z t)
    in
    ((if hOffset == 0 then
        12

      else
        hOffset
     )
        |> S.fromInt
    )
        ++ ":"
        ++ (if secs < 10 then
                "0" ++ secsVis

            else
                secsVis
           )
        ++ " "
        ++ suffix


isValidHash : Int -> String -> Bool
isValidHash target id =
    S.length id >= target && (S.left target id |> S.all ((==) '0'))


viewTimestamp : Int -> Html Msg
viewTimestamp t =
    let
        z =
            customZone (-7 * 60) []
    in
    let
        d =
            t |> (*) 1000 |> millisToPosix
    in
    p [] [ text ((d |> toMonth z |> showMonth) ++ " " ++ (d |> toDay z |> S.fromInt) ++ ", " ++ (d |> toYear z |> S.fromInt) ++ " " ++ (d |> showTime)) ]


getYtEmbed : String -> String
getYtEmbed s =
    S.split "v=" s |> L.drop 1 |> L.head |> M.withDefault "" |> (++) "https://www.youtube.com/embed/"


getYtbeEmbed : String -> String
getYtbeEmbed s =
    S.split ".be/" s |> L.drop 1 |> L.head |> M.withDefault "" |> (++) "https://www.youtube.com/embed/"


getChunkTime : Int -> Int
getChunkTime t =
    t // 86400 * 86400


viewMultimedia : Maybe Multimedia -> Html Msg
viewMultimedia m =
    case m of
        Just media ->
            if S.contains "youtube.com" media.src then
                iframe [ src (getYtEmbed media.src), class "content", width 560, height 315 ] []

            else if S.contains "youtu.be" media.src then
                iframe [ src (getYtbeEmbed media.src), class "content", width 560, height 315 ] []

            else if S.contains "rumble.com/embed" media.src then
                iframe [ src media.src, class "content", width 560, height 315 ] []

            else
                case media.kind of
                    Image ->
                        img [ src media.src, class "content" ] []

                    Video ->
                        video [ src media.src, class "content", preload "metadata", property "muted" (JE.bool True), loop True, controls True ] []

        Nothing ->
            text ""


viewMultimediaSus : Bool -> Maybe Multimedia -> String -> Html Msg
viewMultimediaSus blurred m parentId =
    div [ class "contentContainer", onClick (SetMediaVisible (not blurred) parentId) ]
        [ if blurred then
            img [ src "/hide.svg", class "hideIcon" ] []

          else
            text ""
        , case m of
            Just media ->
                if S.contains "youtube.com" media.src then
                    iframe
                        [ src (getYtEmbed media.src)
                        , class "content"
                        , if blurred then
                            class "blurred"

                          else
                            class ""
                        , width 560
                        , height 315
                        ]
                        []

                else if S.contains "youtu.be" media.src then
                    iframe
                        [ src (getYtbeEmbed media.src)
                        , class "content"
                        , if blurred then
                            class "blurred"

                          else
                            class ""
                        , width 560
                        , height 315
                        ]
                        []

                else if S.contains "rumble.com/embed" media.src then
                    iframe
                        [ src media.src
                        , if blurred then
                            class "blurred"

                          else
                            class ""
                        , class "content"
                        , width 560
                        , height 315
                        ]
                        []

                else
                    case media.kind of
                        Image ->
                            img
                                [ src media.src
                                , if blurred then
                                    class "blurred"

                                  else
                                    class ""
                                , class "content"
                                ]
                                []

                        Video ->
                            video
                                [ src media.src
                                , class "content"
                                , if blurred then
                                    class "blurred"

                                  else
                                    class ""
                                , preload "metadata"
                                , property "muted" (JE.bool True)
                                , loop True
                                , controls True
                                ]
                                []

            Nothing ->
                text ""
        ]


descending a b =
    case compare a.timestamp b.timestamp of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


viewLinkableText : String -> Html Msg
viewLinkableText s =
    div [ class "linkLine" ]
        (s
            |> S.words
            |> L.map
                (\word ->
                    if S.startsWith "http://" word || S.startsWith "https://" word then
                        a [ href word, target "_blank" ] [ text word ]

                    else
                        p [] [ text word ]
                )
        )


viewTextLine : String -> Html Msg
viewTextLine s =
    if s |> S.startsWith ">" then
        p [ class "greentext" ] [ text s ]

    else if S.contains "http" s then
        viewLinkableText s

    else
        p [] [ text s ]


viewPostText : String -> Html Msg
viewPostText p =
    div [ class "postText" ] (p |> S.lines |> L.map viewTextLine)


viewCommentText : String -> Html Msg
viewCommentText c =
    div [ class "commentText" ] (c |> S.lines |> L.map viewTextLine)


viewPost : Bool -> Int -> Bool -> Post -> Html Msg
viewPost blurred nComments verified post =
    div [ class "post" ]
        [ div [ class "postHeader" ]
            [ div [ class "postTitleLine" ]
                [ if verified then
                    img [ src "/verified.svg", class "verifiedIndicator" ] []

                  else
                    text ""
                , h1 [] [ text post.title ]
                , viewTimestamp post.timestamp
                ]
            , div [ class "postActions" ] [ img [ src "/link.svg", onClick (CopyString ("https://dubchan.net/?post=" ++ S.dropRight 1 post.id)) ] [] ]
            ]
        , viewPostText post.text
        , viewMultimediaSus blurred post.content post.id
        , div [ class "postAction", onClick (SelectPost (Just post.id)) ] [ img [ src "/forum.svg" ] [], p [] [ text ("Comments " ++ "(" ++ (nComments |> S.fromInt) ++ ")") ] ]
        ]


viewCommentArea : String -> CommentSubmission -> Html Msg
viewCommentArea feedback submission =
    div [ class "commentDrawerArea" ]
        [ div [ class "commentInputArea" ]
            [ div [ class "commentInputs" ]
                [ textarea [ class "commentInput", placeholder "Post a reply", onInput ChangeSubCommentText, value submission.text ] []
                , div [ class "commentContentInput" ]
                    [ input [ placeholder "Link an attachment", onInput ChangeSubCommentContent, value submission.content ] []
                    , p
                        [ onClick SetSubCommentContentImage
                        , if submission.contentKind == Image then
                            class "active"

                          else
                            class ""
                        ]
                        [ text "image" ]
                    , p
                        [ onClick SetSubCommentContentVideo
                        , if submission.contentKind == Video then
                            class "active"

                          else
                            class ""
                        ]
                        [ text "video" ]
                    ]
                ]
            , div [ class "commentInputActions" ] [ img [ onClick ClearSub, class "cancel", src "/trash.svg" ] [], p [ onClick ValidateComment ] [ text "Submit" ] ]
            ]
        , if feedback /= "" then
            p [ class "feedback" ] [ text feedback ]

          else
            text ""
        ]


viewSubmitPost : String -> Submission -> Html Msg
viewSubmitPost feedback submission =
    div [ class "submitArea" ]
        [ h1 [] [ text "New Post" ]
        , input [ id "titleInput", placeholder "Post Title", onInput ChangeSubTitle, value submission.title ] []
        , div [ class "bodyInputArea" ]
            [ if submission.content /= "" then
                viewMultimedia (Just (Multimedia submission.content submission.contentKind))

              else
                text ""
            , div [ class "bodyInput" ]
                [ textarea [ placeholder "Post Text", onInput ChangeSubText, value submission.text ] []
                , div [ class "mediaSelector" ]
                    [ input [ placeholder "Post Attachment", onInput ChangeSubContent, value submission.content ] []
                    , p
                        [ onClick SetSubContentVideo
                        , if submission.contentKind == Video then
                            class "active"

                          else
                            class ""
                        ]
                        [ text "video" ]
                    , p
                        [ onClick SetSubContentImage
                        , if submission.contentKind == Image then
                            class "active"

                          else
                            class ""
                        ]
                        [ text "image" ]
                    ]
                ]
            ]
        , if feedback /= "" then
            p [ class "feedback" ] [ text ("Error: " ++ feedback) ]

          else
            text ""
        , p [ onClick ValidatePost, class "submit" ] [ text "Submit" ]
        ]
