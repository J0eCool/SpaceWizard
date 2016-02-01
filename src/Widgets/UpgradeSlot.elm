module Widgets.UpgradeSlot where

import Focus
import Html exposing (Html, div, h3, text, span, button, ul, li)
import Html.Events exposing (onMouseDown, onMouseUp, onMouseEnter, onMouseLeave)
import Format
import Style exposing (..)
import Widgets.ProgressBar as ProgressBar

type Action a
    = SetHeld a
    | SetHover a
    | Release
    | MoveOut


viewStat ctx cost action address focus model =
    let
        stat = Focus.get focus model
        title = ctx.title stat
        level = ctx.level stat
        curCost = cost 1 focus model
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
                        ++ Format.currency curCost
                        ++ ")"
                    ]

        next =
            ceiling <| level + 1.0e-6

        toNext =
            toFloat next - level
    in
        li
            []
            [ span
                [ style
                    [ display InlineBlock
                    , width <| Px 120
                    ]
                ]
                [ text
                    <| title
                    ++ ": "
                    ++ Format.float level
                ]
            , upgradeButton 1 (action focus)
            , div [] [ProgressBar.xpBar level]
            ]

