module Style (..) where

import Char exposing (toCode, fromCode)
import Color exposing (Color, toRgb)
import Html
import Html.Attributes as Attr
import String


type alias Style =
    ( String, String )


baseStyle : String -> (a -> String) -> a -> Style
baseStyle str f x =
    ( str, f x )


style : List Style -> Html.Attribute
style =
    Attr.style



-----------------------------------


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
width =
    baseStyle "width" sizeStr


height : Size number -> Style
height =
    baseStyle "height" sizeStr


type alias MarginInput a =
    { left : Size a
    , right : Size a
    , top : Size a
    , bottom : Size a
    }


margin : MarginInput number -> Style
margin input =
    margin4 input.top input.right input.bottom input.left


margin4 : Size number -> Size number -> Size number -> Size number -> Style
margin4 top right bottom left =
    let
        sizes =
            [ top, right, bottom, left ]
                |> List.map sizeStr
                |> List.intersperse " "
                |> String.concat
    in
        ( "margin", sizes )


margin2 : Size number -> Size number -> Style
margin2 vertical horizontal =
    margin4 vertical horizontal vertical horizontal


margin1 : Size number -> Style
margin1 m =
    margin4 m m m m



-----------------------------------


colorStr : Color -> String
colorStr color =
    let
        { red, green, blue } =
            toRgb color

        contents =
            [ red, green, blue ]
                |> List.map toString
                |> List.intersperse ", "
                |> String.concat
    in
        "rgb(" ++ contents ++ ")"


color : Color -> Style
color =
    baseStyle "color" colorStr


backgroundColor : Color -> Style
backgroundColor =
    baseStyle "background-color" colorStr



-----------------------------------


type Display
    = None
    | Inline
    | Block
    | InlineBlock


displayStr : Display -> String
displayStr d =
    case d of
        None ->
            "none"

        Inline ->
            "inline"

        Block ->
            "block"

        InlineBlock ->
            "inline-block"


display : Display -> Style
display =
    baseStyle "display" displayStr



-----------------------------------


type VerticalAlign
    = Top
    | Middle
    | Bottom


verticalAlignStr : VerticalAlign -> String
verticalAlignStr align =
    case align of
        Top ->
            "top"

        Middle ->
            "middle"

        Bottom ->
            "bottom"


verticalAlign : VerticalAlign -> Style
verticalAlign =
    baseStyle "vertical-align" verticalAlignStr



-----------------------------------


type FontWeight
    = NormalWeight
    | Bold


fontWeightStr : FontWeight -> String
fontWeightStr weight =
    case weight of
        NormalWeight ->
            "normal"

        Bold ->
            "bold"


fontWeight : FontWeight -> Style
fontWeight =
    baseStyle "font-weight" fontWeightStr


type FontStyle
    = NormalStyle
    | Italic


fontStyleStr : FontStyle -> String
fontStyleStr style =
    case style of
        NormalStyle ->
            "normal"

        Italic ->
            "italic"


fontStyle : FontStyle -> Style
fontStyle =
    baseStyle "font-style" fontStyleStr



-----------------------------------


backgroundImage : String -> Style
backgroundImage url =
    ( "background-image", "url('" ++ url ++ "')" )


pixelated : Style
pixelated =
    ( "image-rendering", "pixelated" )


flipHorizontal : Style
flipHorizontal =
    ( "transform", "scaleX(-1)" )



-------------------------


backgroundImage : String -> Style
backgroundImage url =
    ( "background-image", "url('" ++ url ++ "')" )


pixelated : Style
pixelated =
    ( "image-rendering", "pixelated" )


flipHorizontal : Style
flipHorizontal =
    ( "transform", "scaleX(-1)" )
