module Widgets.ProgressBar where

import Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (style)

import Style exposing (..)

type alias Model =
    { width : Int
    , height : Int
    , curAmount : Float
    , maxAmount : Float
    , color : Color
    , background : Color
    }

view : Model -> Html
view bar =
    let fraction = bar.curAmount / bar.maxAmount
        foreground =
            div
                [ style
                    [ width <| Pct <| 100 * fraction
                    , height <| Pct 100
                    , backgroundColor <| bar.color
                    ]
                ]
                []
        background = 
            div 
                [ style
                    [ width <| Px bar.width
                    , height <| Px bar.height
                    , backgroundColor <| bar.background
                    ]
                ]
                [ foreground
                ]
    in background
