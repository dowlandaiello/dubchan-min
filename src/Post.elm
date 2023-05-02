module Post exposing (..)

import Hash exposing (Hash)
import Html exposing (Html, div, h1, iframe, img, input, label, p, text, textarea, video)
import Html.Attributes exposing (class, for, height, id, placeholder, src, title, width)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder, Error, field, int, list, map2, map3, map4, map6, nullable, string)
import Json.Encode as JE
import List as L
import Maybe as M
import Msg exposing (Msg(..))
import String as S
import Time exposing (Month(..), Posix, Zone, customZone, millisToPosix, posixToMillis, toDay, toHour, toMinute, toMonth, toYear)


type alias Post =
    { timestamp : Int
    , title : String
    , text : String
    , content : Maybe Multimedia
    , comments : Maybe (List Comment)
    , id : String
    }


type alias Submission =
    { title : String
    , text : String
    , content : String
    , contentKind : MultimediaKind
    }


type alias CommentSubmission =
    { text : String
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


fromSubmission : Posix -> Submission -> Post
fromSubmission time sub =
    Post (posixToMillis time // 1000)
        sub.title
        sub.text
        (if sub.content /= "" then
            Just (Multimedia sub.content sub.contentKind)

         else
            Nothing
        )
        Nothing
        (postId time sub |> Hash.toString)


commentFromSubmission : String -> Posix -> CommentSubmission -> Comment
commentFromSubmission parent time sub =
    Comment (posixToMillis time // 1000) sub.text parent


type alias Comment =
    { timestamp : Int
    , text : String
    , parent : String
    }


type alias Multimedia =
    { src : String
    , kind : MultimediaKind
    }


type MultimediaKind
    = Image
    | Video


commentDecoder : Decoder Comment
commentDecoder =
    map3 Comment (field "timestamp" int) (field "text" string) (field "parent" string)


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
    JE.object [ ( "timestamp", JE.int p.timestamp ), ( "title", JE.string p.title ), ( "text", JE.string p.text ), ( "content", p.content |> M.map multimediaEncoder |> M.withDefault JE.null ), ( "comments", JE.null ), ( "id", JE.string p.id ) ]


commentEncoder : Comment -> JE.Value
commentEncoder c =
    JE.object [ ( "timestamp", JE.int c.timestamp ), ( "text", JE.string c.text ), ( "parent", JE.string c.parent ) ]


postId : Posix -> Submission -> Hash
postId t sub =
    Hash.dependent (Hash.fromString sub.title) (Hash.fromString sub.text) |> Hash.dependent (Hash.fromInt (t |> posixToMillis))


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
    map6 Post (field "timestamp" int) (field "title" string) (field "text" string) (field "content" (nullable multimediaDecoder)) (field "comments" (nullable (list commentDecoder))) (field "id" string)


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
    (modBy 12 (toHour z t) |> S.fromInt)
        ++ ":"
        ++ (if secs < 10 then
                "0" ++ secsVis

            else
                secsVis
           )
        ++ " "
        ++ suffix


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


viewMultimedia : Maybe Multimedia -> Html Msg
viewMultimedia m =
    case m of
        Just media ->
            if S.contains "youtube.com" media.src then
                iframe [ src (getYtEmbed media.src), class "content", width 560, height 315 ] []

            else
                case media.kind of
                    Image ->
                        img [ src media.src, class "content" ] []

                    Video ->
                        video [ src media.src, class "content" ] []

        Nothing ->
            text ""


descending a b =
    case compare a.timestamp b.timestamp of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


viewTextLine : String -> Html Msg
viewTextLine s =
    if s |> S.startsWith ">" then
        p [ class "greentext" ] [ text s ]

    else
        p [] [ text s ]


viewPostText : String -> Html Msg
viewPostText p =
    div [ class "postText" ] (p |> S.lines |> L.map viewTextLine)


viewCommentText : String -> Html Msg
viewCommentText c =
    div [ class "commentText" ] (c |> S.lines |> L.map viewTextLine)


viewPost : Post -> Html Msg
viewPost post =
    div [ class "post" ] [ div [ class "postTitleLine" ] [ h1 [] [ text post.title ], viewTimestamp post.timestamp ], viewPostText post.text, viewMultimedia post.content, div [ class "postAction", onClick (SelectPost (Just post.id)) ] [ img [ src "/forum.svg" ] [], p [] [ text "Comments" ] ] ]


viewComment : Comment -> Html Msg
viewComment comment =
    div [ class "comment" ] [ p [ class "commentTimestamp" ] [ viewTimestamp comment.timestamp ], viewCommentText comment.text ]


viewPostComments : List Comment -> Html Msg
viewPostComments comments =
    div [ class "comments" ] (comments |> L.sortWith descending |> L.filter (\comment -> comment.text /= "") |> L.map viewComment)


viewCommentArea : Html Msg
viewCommentArea =
    div [ class "commentInputArea" ] [ textarea [ class "commentInput", placeholder "Post a reply", onInput ChangeSubCommentText ] [], p [ onClick SubmitComment ] [ text "Submit" ] ]


viewPosts : List Post -> Html Msg
viewPosts posts =
    div [] (posts |> L.sortWith descending |> L.filter (\post -> post.text /= "") |> L.map viewPost)


viewSubmitPost : MultimediaKind -> Html Msg
viewSubmitPost activeKind =
    div [ class "submitArea" ]
        [ h1 [] [ text "New Post" ]
        , input [ id "titleInput", placeholder "Post Title", onInput ChangeSubTitle ] []
        , textarea [ placeholder "Post Text", onInput ChangeSubText ] []
        , div [ class "mediaSelector" ]
            [ input [ placeholder "Post Attachment", onInput ChangeSubContent ] []
            , p
                [ onClick SetSubContentVideo
                , if activeKind == Video then
                    class "active"

                  else
                    class ""
                ]
                [ text "video" ]
            , p
                [ onClick SetSubContentImage
                , if activeKind == Image then
                    class "active"

                  else
                    class ""
                ]
                [ text "image" ]
            ]
        , p [ onClick SubmitPost, class "submit" ] [ text "Submit" ]
        ]
