module Mana (..) where

import Color
import Html exposing (Html, div, span, h3, text, ul, li, button)
import Html.Events exposing (onClick)
import Format
import Widgets.ProgressBar as ProgressBar


type alias Model =
    { current : Float
    , capacityLevel : Float
    , regenLevel : Float
    }

type Action
    = Tick Float


init : Model
init =
    { current = 0
    , capacityLevel = 0
    , regenLevel = 0
    }

update : Action -> Model -> Model
update action model =
    case action of
        Tick dT ->
            let
                r = regen model
                cur = model.current + r * dT
                    |> min (toFloat (capacity model))
            in { model | current = cur }

view : Signal.Address Action -> Model -> Html
view address model =
    let manaBar =
            { width = 600
            , height = 12
            , curAmount = model.current
            , maxAmount = toFloat <| capacity model
            , color = Color.rgb 16 92 240
            , background = Color.rgb 16 16 92
            }
    in div []
        [ h3 [] [text "Mana"]
        , text <| Format.int (floor model.current) ++ "/" ++ Format.int (capacity model)
        , ProgressBar.view manaBar
        ]

capacity : Model -> Int
capacity model =
    let lv = model.capacityLevel
    in floor <| 1000 + 50 * lv

regen : Model -> Float
regen model =
    let lv = model.regenLevel
    in 0.8 + 0.1 * lv
