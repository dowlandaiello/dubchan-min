module Theme exposing (..)

import Dict as D
import Json.Decode as JD
import String as S


type alias Theme =
    { name : String
    }


defaultTheme : Theme
defaultTheme =
    { name = "Default" }


themes : D.Dict String Theme
themes =
    D.fromList [ ( "Default", defaultTheme ), ( "Demon", { name = "Demon" } ), ( "Suit", { name = "Suit" } ), ( "Touch Grass", { name = "Touch Grass" } ) ]


themeClass : Theme -> String
themeClass =
    .name >> S.replace " " ""


themeDecoder : JD.Decoder Theme
themeDecoder =
    JD.map Theme (JD.field "name" JD.string)
