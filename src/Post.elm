module Post exposing (..)

import Captcha exposing (Captcha, captchaDecoder, captchaEncoder)
import Flip exposing (flip)
import Hash exposing (Hash)
import Html exposing (Attribute, Html, a, div, h1, iframe, img, input, label, p, text, textarea, video)
import Html.Attributes exposing (checked, class, controls, for, height, href, id, loop, maxlength, placeholder, preload, property, src, target, title, type_, value, width)
import Html.Events exposing (on, onClick, onInput)
import Identity exposing (..)
import Json.Decode as JD exposing (Decoder, Error, field, float, int, list, map2, map3, map5, map7, map8, maybe, nullable, string)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import Json.Encode.Optional as Opt
import List as L
import Maybe as M
import Msg exposing (Conversation, Msg(..))
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
    , tripcode : Maybe String
    , pubKey : Maybe String
    , sig : Maybe String
    , encPubKey : Maybe String
    }


type alias Submission =
    { title : String
    , text : String
    , content : String
    , nonce : Int
    , contentKind : MultimediaKind
    , tripcode : Maybe String
    , pubKey : Maybe String
    , encPubKey : Maybe String
    }


type alias CommentSubmission =
    { text : String
    , parent : String
    , content : String
    , contentKind : MultimediaKind
    , nonce : Int
    , tripcode : Maybe String
    , pubKey : Maybe String
    , encPubKey : Maybe String
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


setCommentSubTripcode : Maybe String -> CommentSubmission -> CommentSubmission
setCommentSubTripcode s submission =
    { submission | tripcode = s }


setTripcode : Maybe String -> Submission -> Submission
setTripcode s submission =
    { submission | tripcode = s }


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
    { text = c.text, parent = c.parent, content = content.src, contentKind = content.kind, nonce = c.nonce, tripcode = c.tripcode, pubKey = c.pubKey, encPubKey = c.encPubKey }


submissionFromPost : Post -> Submission
submissionFromPost p =
    let
        content =
            M.withDefault (Multimedia "" Image) p.content
    in
    { title = p.title, text = p.text, content = content.src, nonce = p.nonce, contentKind = content.kind, pubKey = p.pubKey, tripcode = p.tripcode, encPubKey = p.encPubKey }


fromSubmission : Captcha -> String -> Int -> Posix -> Submission -> Post
fromSubmission captcha prev target time sub =
    let
        id =
            postId time sub
    in
    if isValidHash target id then
        { timestamp = posixToMillis time // 1000
        , title = sub.title
        , text = sub.text
        , content =
            if sub.content /= "" then
                Just (Multimedia sub.content sub.contentKind)

            else
                Nothing
        , comments = Nothing
        , nonce = sub.nonce
        , captcha = Just captcha
        , captchaAnswer = Nothing
        , id = id
        , hash = id
        , prev = Just prev
        , tripcode = sub.tripcode
        , pubKey = sub.pubKey
        , sig = Nothing
        , encPubKey = sub.encPubKey
        }

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
        { timestamp = posixToMillis time // 1000
        , text = sub.text
        , parent = sub.parent
        , id = id
        , content =
            if sub.content /= "" then
                Just (Multimedia sub.content sub.contentKind)

            else
                Nothing
        , nonce = 0
        , captchaAnswer = Nothing
        , hash = id
        , tripcode = sub.tripcode
        , pubKey = sub.pubKey
        , sig = Nothing
        , encPubKey = sub.encPubKey
        }

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
    , captchaAnswer : Maybe String
    , hash : String
    , tripcode : Maybe String
    , pubKey : Maybe String
    , sig : Maybe String
    , encPubKey : Maybe String
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
    JD.succeed Comment
        |> andMap (field "timestamp" int)
        |> andMap (field "text" string)
        |> andMap (field "parent" string)
        |> andMap (field "id" string)
        |> andMap (field "content" (nullable multimediaDecoder))
        |> andMap (field "nonce" int)
        |> andMap (maybe (field "captchaAnswer" string))
        |> andMap (field "hash" string)
        |> andMap (maybe (field "tripcode" string))
        |> andMap (maybe (field "pubKey" string))
        |> andMap (maybe (field "sig" string))
        |> andMap (maybe (field "encPubKey" string))


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
    [ ( "timestamp", p.timestamp ) |> Opt.field JE.int
    , ( "title", p.title ) |> Opt.field JE.string
    , ( "text", p.text ) |> Opt.field JE.string
    , ( "content", p.content |> M.map multimediaEncoder |> M.withDefault JE.null ) |> Opt.field identity
    , ( "comments", JE.null ) |> Opt.field identity
    , ( "nonce", p.nonce ) |> Opt.field JE.int
    , ( "hash", p.hash ) |> Opt.field JE.string
    , ( "id", p.id ) |> Opt.field JE.string
    , ( "prev", p.prev ) |> Opt.optionalField JE.string
    , ( "captcha", p.captcha |> M.map captchaEncoder ) |> Opt.optionalField identity
    , ( "captchaAnswer", p.captchaAnswer ) |> Opt.optionalField JE.string
    , ( "pubKey", p.pubKey ) |> Opt.optionalField JE.string
    , ( "sig", p.sig ) |> Opt.optionalField JE.string
    , ( "tripcode", p.tripcode ) |> Opt.optionalField JE.string
    , ( "encPubKey", p.encPubKey ) |> Opt.optionalField JE.string
    ]
        |> Opt.objectMaySkip


commentEncoder : Comment -> JE.Value
commentEncoder c =
    [ ( "timestamp", c.timestamp ) |> Opt.field JE.int
    , ( "text", c.text ) |> Opt.field JE.string
    , ( "parent", c.parent ) |> Opt.field JE.string
    , ( "id", c.id ) |> Opt.field JE.string
    , ( "content", c.content |> M.map multimediaEncoder |> M.withDefault JE.null ) |> Opt.field identity
    , ( "nonce", c.nonce ) |> Opt.field JE.int
    , ( "hash", c.hash ) |> Opt.field JE.string
    , ( "captchaAnswer", c.captchaAnswer ) |> Opt.optionalField JE.string
    , ( "pubKey", c.pubKey ) |> Opt.optionalField JE.string
    , ( "sig", c.sig ) |> Opt.optionalField JE.string
    , ( "tripcode", c.tripcode ) |> Opt.optionalField JE.string
    , ( "encPubKey", c.encPubKey ) |> Opt.optionalField JE.string
    ]
        |> Opt.objectMaySkip


postId : Posix -> Submission -> String
postId t sub =
    sha256 (sub.title ++ sub.text ++ S.fromInt sub.nonce ++ S.fromInt (posixToMillis t // 1000) ++ (sub.tripcode |> M.withDefault ""))


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
        |> andMap (maybe (field "tripcode" string))
        |> andMap (maybe (field "pubKey" string))
        |> andMap (maybe (field "sig" string))
        |> andMap (maybe (field "encPubKey" string))


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


viewMultimedia : Maybe (Decoder Msg) -> Maybe (Decoder Msg) -> Maybe Multimedia -> Html Msg
viewMultimedia onError onLoad m =
    let
        eventAttrs =
            [ onError |> M.map (on "error"), onLoad |> M.map (on "load") ] |> L.filterMap identity
    in
    case m of
        Just media ->
            if S.contains "youtube.com" media.src then
                iframe ([ src (getYtEmbed media.src), class "content", width 560, height 315 ] ++ eventAttrs) []

            else if S.contains "youtu.be" media.src then
                iframe ([ src (getYtbeEmbed media.src), class "content", width 560, height 315 ] ++ eventAttrs) []

            else if S.contains "rumble.com/embed" media.src then
                iframe ([ src media.src, class "content", width 560, height 315 ] ++ eventAttrs) []

            else
                case media.kind of
                    Image ->
                        img ([ src media.src, class "content" ] ++ eventAttrs) []

                    Video ->
                        video ([ src media.src, class "content", preload "metadata", property "muted" (JE.bool True), loop True, controls True ] ++ eventAttrs) []

        Nothing ->
            text ""


viewMultimediaSub : Maybe Multimedia -> Html Msg
viewMultimediaSub m =
    div [ class "multimediaSub" ] [ viewMultimedia (Just (JD.succeed (SetSubContentValid False))) (Just (JD.succeed (SetSubContentValid True))) m ]


viewExpandableMultimedia : Bool -> Maybe Multimedia -> String -> Html Msg
viewExpandableMultimedia expanded m parentId =
    case m of
        Just media ->
            div [ class "contentContainer", onClick (SetMediaExpanded (not expanded) parentId) ] [ viewMultimedia Nothing Nothing m ]

        Nothing ->
            text ""


viewMultimediaSus : Bool -> Maybe Multimedia -> String -> Html Msg
viewMultimediaSus blurred m parentId =
    case m of
        Just media ->
            div [ class "contentContainer", onClick (SetMediaVisible (not blurred) parentId) ]
                [ if blurred then
                    img [ src "/hide.svg", class "hideIcon" ] []

                  else
                    text ""
                , if S.contains "youtube.com" media.src then
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
                ]

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
        , case post.pubKey of
            Just pubKey ->
                if post.timestamp > 1685229313 then
                    ([ p [ class "postAuthor" ] [ text ((post.tripcode |> M.map ((++) "@") |> M.withDefault "") ++ "#" ++ identityShortcode pubKey) ] ] ++ (post.encPubKey |> M.map (Conversation post.tripcode pubKey >> OpenConvo >> onClick >> L.singleton >> (++) [ src "/message.svg" ] >> flip img [] >> L.singleton) |> M.withDefault [])) |> div [ class "authorLine" ]

                else
                    text ""

            Nothing ->
                text ""
        , viewPostText post.text
        , viewMultimediaSus blurred post.content post.id
        , div [ class "postAction", onClick (GoToPost (Just post.id)) ] [ img [ src "/forum.svg", class "accentedSvg" ] [], p [] [ text ("Comments " ++ "(" ++ (nComments |> S.fromInt) ++ ")") ] ]
        ]


viewCommentArea : Maybe Identity -> List Identity -> String -> String -> String -> CommentSubmission -> Html Msg
viewCommentArea activeIdentity identities captcha captchaAnswer feedback submission =
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
                , viewIdSelector True
                    identities
                    activeIdentity
                    (\s ->
                        if S.isEmpty s then
                            ChangeSubCommentTripcode Nothing

                        else
                            ChangeSubCommentTripcode (Just s)
                    )
                , if captcha /= "" then
                    div [ class "commentCaptchaSection" ]
                        [ img [ src captcha ] [], input [ placeholder "Captcha answer", onInput ChangeSubCommentCaptchaAnswer, value captchaAnswer ] [] ]

                  else
                    text ""
                ]
            , div [ class "commentInputActions" ]
                [ img [ onClick ClearSub, class "cancel", src "/trash.svg" ] []
                , if captcha /= "" then
                    img [ onClick RegenCommentCaptcha, class "refresh", src "/refresh.svg" ] []

                  else
                    text ""
                , if captcha /= "" then
                    p [ onClick SubmitComment ] [ text "Submit" ]

                  else
                    p [ onClick ValidateComment ] [ text "Next" ]
                ]
            ]
        , if feedback /= "" then
            p [ class "feedback" ] [ text feedback ]

          else
            text ""
        ]


viewIdSelector : Bool -> List Identity -> Maybe Identity -> (String -> Msg) -> Html Msg
viewIdSelector anonAllowed allIdentities activeIdentity onAliasChange =
    let
        identities =
            case activeIdentity of
                Just identity ->
                    identity :: (allIdentities |> L.filter (\iden -> iden.pubKey /= identity.pubKey))

                Nothing ->
                    allIdentities
    in
    let
        idenInputMin =
            if anonAllowed then
                [ div [ class "idenInputRow" ]
                    [ input
                        [ class "anonCheckbox"
                        , type_ "checkbox"
                        , checked (activeIdentity == Nothing)
                        , onClick
                            (if activeIdentity /= Nothing then
                                ChangeSubIdentity Nothing

                             else
                                ChangeSubIdentity (identities |> L.map .pubKey |> L.head)
                            )
                        ]
                        []
                    , p [ class "anonLabel" ] [ text "Anonymous?" ]
                    ]
                ]

            else
                []
    in
    div [ class "identityInput" ]
        (if activeIdentity == Nothing then
            idenInputMin

         else
            idenInputMin
                ++ [ div [ class "idenInputRow" ]
                        [ input
                            [ class "tripInput"
                            , placeholder "Alias"
                            , onInput
                                onAliasChange
                            , maxlength 20
                            ]
                            []
                        , p [ class "identitySelectorLabel" ] [ text "ID:" ]
                        , viewIdentitySelector
                            (case activeIdentity of
                                Just iden ->
                                    iden :: (identities |> L.filter (\id -> id.pubKey /= iden.pubKey))

                                Nothing ->
                                    identities
                            )
                        ]
                   , p [ class "newIdentity", onClick GenerateIdentity ] [ text "New ID" ]
                   ]
        )


viewSubmitPost : List Identity -> Maybe Identity -> String -> String -> String -> Submission -> Html Msg
viewSubmitPost identities activeIdentity captcha captchaAnswer feedback submission =
    div [ class "submitArea" ]
        [ h1 [] [ text "New Post" ]
        , input [ id "titleInput", placeholder "Post Title", onInput ChangeSubTitle, value submission.title ] []
        , div [ class "bodyInputArea" ]
            [ if submission.content /= "" then
                viewMultimediaSub (Just (Multimedia submission.content submission.contentKind))

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
                , viewIdSelector True
                    identities
                    activeIdentity
                    (\s ->
                        if S.isEmpty s then
                            ChangeSubTripcode Nothing

                        else
                            ChangeSubTripcode (Just s)
                    )
                ]
            ]
        , if captcha /= "" then
            div [ class "captchaInput" ] [ img [ src captcha ] [], input [ placeholder "Captcha answer", onInput ChangeSubCaptchaAnswer, value captchaAnswer ] [] ]

          else
            text ""
        , if feedback /= "" then
            p [ class "feedback" ] [ text ("Error: " ++ feedback) ]

          else
            text ""
        , div [ class "submitActions" ]
            [ if captcha /= "" then
                img [ class "refresh", src "/refresh.svg", onClick RegenPostCaptcha ] []

              else
                text ""
            , if captcha /= "" then
                p [ onClick SubmitPost, class "submit" ] [ text "Submit" ]

              else
                p [ onClick ValidatePost, class "submit" ] [ text "Next" ]
            ]
        ]
