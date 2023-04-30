module Route exposing (..)

import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string)


type Route
    = Post String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf [ map Post (s "post" </> string) ]
