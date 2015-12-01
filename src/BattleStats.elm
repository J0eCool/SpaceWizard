module BattleStats where

import Html exposing (div, h3, text, span, button, ul, li)
import Html.Events exposing (onClick)

import Currency
import Format

type alias Model =
    { strength : Int
    , speed : Int
    }

type Action
    = UpgradeStrength
    | UpgradeSpeed

attackDamage : Model -> Int
attackDamage model =
    (model.strength - 1) + 10

attackSpeed : Model -> Float
attackSpeed model =
    toFloat (model.speed - 1) * 0.1 + 1.2

init : Model
init = 
    { strength = 1
    , speed = 1
    }

update : Action -> Model -> Model
update action model =
    case action of
        UpgradeStrength ->
            { model | strength = model.strength + 1 }
        UpgradeSpeed ->
            { model | speed = model.speed + 1 }

view : Signal.Address (Currency.Bundle, Action) -> Model -> Html.Html
view address model =
    let cost =
            ( Currency.Gold
            , model.strength * 4 + (model.strength - 1) ^ 2
            )
        speedCost =
            ( Currency.Gold
            , model.speed * 7 + ((model.speed - 1) ^ 2) * 5
            )
    in div []
        [ h3 [] [text "Stats"]
        , ul []
            [ li []
                [ span [] [text <| "Strength: " ++ Format.int model.strength]
                , button
                    [onClick address (cost, UpgradeStrength)]
                    [text <| "+1 (" ++ Format.currency cost ++ ")"]
                ]
            , li []
                [ span [] [text <| "Speed: " ++ Format.int model.speed]
                , button
                    [onClick address (speedCost, UpgradeSpeed)]
                    [text <| "+1 (" ++ Format.currency speedCost ++ ")"]
                ]
            ]
        ]
