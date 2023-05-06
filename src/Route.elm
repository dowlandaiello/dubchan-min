module Route exposing (..)

import Url
import Url.Parser exposing ((<?>), Parser, map, oneOf, s, string)
import Url.Parser.Query as Query


type Route
    = Post (Maybe String)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf [ map Post (s "/" <?> Query.string "post") ]
