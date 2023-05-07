module Route exposing (..)

import Url
import Url.Parser exposing ((<?>), Parser, map, oneOf, s, string)
import Url.Parser.Query as Query


routeParser =
    Query.string "post"
