module BattleStats where

import Html exposing (div, h3, text, span, button, ul, li)
import Html.Events exposing (onClick)

import Currency
import Format

type alias Model =
    { attackSpeed : Float
    , attackDamage : Int
    }

type Action
    = UpgradeDamage
    | UpgradeSpeed

init : Model
init = 
    { attackSpeed = 1.2
    , attackDamage = 10
    }

update : Action -> Model -> Model
update action model =
    case action of
        UpgradeDamage ->
            { model | attackDamage = model.attackDamage + 1 }
        UpgradeSpeed ->
            { model | attackSpeed = model.attackSpeed + 0.1 }

view : Signal.Address (Currency.Bundle, Action) -> Model -> Html.Html
view address model =
    let cost =
            ( Currency.Gold
            , (model.attackDamage - 9) * 4 + (model.attackDamage - 10) ^ 2
            )
        speedCost =
            ( Currency.Gold
            , floor <| (model.attackSpeed * 10 - 11) * 7 + ((model.attackSpeed * 10 - 12) ^ 2) * 5
            )
    in div []
        [ h3 [] [text "Stats"]
        , ul []
            [ li []
                [ span [] [text <| "Damage: " ++ Format.int model.attackDamage]
                , button
                    [onClick address (cost, UpgradeDamage)]
                    [text <| "+1 (" ++ Format.currency cost ++ ")"]
                ]
            , li []
                [ span [] [text <| "Attack Speed: " ++ Format.float model.attackSpeed]
                , button
                    [onClick address (speedCost, UpgradeSpeed)]
                    [text <| "+0.1 (" ++ Format.currency speedCost ++ ")"]
                ]
            ]
        ]
