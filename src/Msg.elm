module Msg exposing (..)

import Browser
import Json.Decode as JD
import Time
import Url


type Msg
    = SelectPost String
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
