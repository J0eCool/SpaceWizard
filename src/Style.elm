module Style where

import Char exposing (toCode, fromCode)
import Color
import String
import Vendor

type alias Style =
    (String, String)

----------------------------------

type Size n
    = Pixels n
    | Percent n

px : number -> Size number
px n = Pixels n

pct : number -> Size number
pct n = Percent n

sizeStr : Size number -> String
sizeStr size =
    case size of
        Pixels pix ->
            toString pix ++ "px"
        Percent pct ->
            toString pct ++ "%"

width : Size number -> Style
width w =
    ("width", sizeStr w)

height : Size number -> Style
height h =
    ("height", sizeStr h)

----------------------------------

colorStr : Color.Color -> String
colorStr color =
    let { red, green, blue } = Color.toRgb color
    in "rgb(" ++ toString red ++ ", " ++ toString green ++ ", " ++ toString blue ++ ")"

backgroundColor : Color.Color -> Style
backgroundColor color =
    ("background-color", colorStr color)
