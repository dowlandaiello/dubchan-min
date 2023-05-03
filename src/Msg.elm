module Msg exposing (..)

import Browser
import Json.Decode as JD
import Time
import Url


type Msg
    = SelectPost (Maybe String)
    | ClickLink Browser.UrlRequest
    | ChangeUrl Url.Url
    | PostLoaded JD.Value
    | PostAdded JD.Value
    | ChangeSubTitle String
    | ChangeSubText String
    | ChangeSubContent String
    | SetSubContentImage
    | SetSubContentVideo
    | SubmitPost
    | Tick Time.Posix
    | ChangeSubCommentText String
    | SubmitComment
    | CommentAdded JD.Value
    | ChangeSubParent String
    | ClearSub
    | ToggleHideChain String
    | ChangeSearchQuery String
