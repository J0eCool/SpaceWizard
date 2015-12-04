module Widgets.ProgressBar where

import Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (style)

import Style exposing (..)

type alias Model =
    { w : Int
    , h : Int
    , curAmount : Float
    , maxAmount : Float
    , bgColor : Color
    , fgColor : Color
    }

view : Model -> Html
view bar =
    let fraction = bar.curAmount / bar.maxAmount
        foreground =
            div
                [ style
                    [ width <| Pct <| 100 * fraction
                    , height <| Pct 100
                    , backgroundColor <| bar.fgColor
                    ]
                ]
                []
        background = 
            div 
                [ style
                    [ width <| Px bar.w
                    , height <| Px bar.h
                    , backgroundColor <| bar.bgColor
                    ]
                ]
                [ foreground
                ]
    in background
