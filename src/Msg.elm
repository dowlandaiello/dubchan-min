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
    | ValidatePost
    | SubmitPost
    | Tick Time.Posix
    | ChangeSubCommentText String
    | ChangeSubCommentContent String
    | SetSubCommentContentImage
    | SetSubCommentContentVideo
    | ValidateComment
    | SubmitComment
    | CommentAdded JD.Value
    | ChangeSubParent String
    | ClearSub
    | ToggleHideChain String
    | ChangeSearchQuery String
    | SetMediaVisible Bool String
    | ToggleBlurImages
    | CopyString String
    | ScrolledBottom
    | GotCaptcha JD.Value
    | NewQuote String
    | RefreshPostCaptcha
    | RefreshCommentCaptcha
    | LoadedCaptcha JD.Value
    | ChangeSubCaptchaAnswer String
    | ChangeSubCommentCaptchaAnswer String
    | SetSubContentValid Bool
    | SetMediaExpanded Bool String
