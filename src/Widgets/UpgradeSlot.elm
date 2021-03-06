module Widgets.UpgradeSlot (..) where

import Focus exposing (Focus)
import Html exposing (Html, div, h3, text, span, button, ul, li)
import Html.Events exposing (onMouseDown, onMouseUp, onMouseEnter, onMouseLeave)
import Format
import Style exposing (..)
import Widgets.LevelText as LevelText
import Widgets.ProgressBar as ProgressBar


type Action a
  = SetHeld a
  | SetHover a
  | Release
  | MoveOut


type alias Context model cost =
  { title : model -> String
  , level : model -> Float
  , format : cost -> String
  , elem : List Html.Attribute -> List Html -> Html
  }


type alias CostFunc model cost =
  Float -> Focus model Float -> model -> cost



--viewStat : Context stat cost -> CostFunc stat cost -> (Focus model stat -> action)
--    -> Signal.Address (Action action)
--    -> Focus model stat -> model -> Html


viewStat ctx cost action address focus model =
  let
    stat =
      Focus.get focus model

    title =
      ctx.title stat

    level =
      ctx.level stat

    curCost =
      cost 1 focus model

    upgradeButton delta act =
      let
        curCost =
          cost delta focus model
      in
        button
          [ onMouseDown address <| SetHeld act
          , onMouseEnter address <| SetHover act
          , onMouseUp address <| Release
          , onMouseLeave address <| MoveOut
          ]
          [ text
              <| "+1 ("
              ++ ctx.format curCost
              ++ ")"
          ]

    next =
      ceiling <| level + 1.0e-6

    toNext =
      toFloat next - level
  in
    ctx.elem
      []
      [ span
          [ style
              [ display InlineBlock
              , width <| Px 120
              ]
          ]
          [ span
              []
              [ text
                  <| title
                  ++ ": "
              ]
          , LevelText.view level
          ]
      , upgradeButton 1 (action focus)
      , div [] [ ProgressBar.xpBar level ]
      ]
