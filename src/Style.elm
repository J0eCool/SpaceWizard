module Style where

import Char exposing (toCode, fromCode)
import Color
import String
import Vendor

type alias Style =
    (String, String)

----------------------------------

type Size n
    = Px n
    | Pct n

sizeStr : Size number -> String
sizeStr size =
    case size of
        Px pix ->
            toString pix ++ "px"
        Pct pct ->
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
