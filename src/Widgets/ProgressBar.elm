module Widgets.ProgressBar where

import Color exposing (Color)
import Html exposing (Html, div)

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
view =
  baseView Block

viewInline : Model -> Html
viewInline =
  baseView InlineBlock

baseView : Display -> Model -> Html
baseView displayType bar =
  let
    fraction = bar.curAmount / bar.maxAmount
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
          [ display displayType
          , verticalAlign Middle
          , margin1 <| Px 4
          , width <| Px bar.width
          , height <| Px bar.height
          , backgroundColor <| bar.background
          ]
        ]
        [ foreground
        ]
  in background

xpBar : Float -> Html
xpBar level =
  viewInline
    { width = 200
    , height = 10
    , color = Color.rgb 64 255 200
    , background = Color.rgb 32 128 92
    , curAmount = level - toFloat (floor level)
    , maxAmount = 1
    }
