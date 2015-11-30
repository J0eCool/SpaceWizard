module Battle where

import Html exposing (div, h3, text, span, button)
import Html.Events exposing (onClick)

import Currency
import Format

type alias Model =
    { health : Int
    , maxHealth : Int
    , attackTimer : Float
    , attackSpeed : Float
    , attackDamage : Int
    , goldReward : Int
    }

type Action
    = Tick Float
    | Upgrade

init : Model
init =
    { health = 100
    , maxHealth = 100
    , attackTimer = 0
    , attackSpeed = 1.2
    , attackDamage = 10
    , goldReward = 8
    }

update : Action -> Model -> (Model, List Currency.Bundle)
update action model =
    let no m = (m, [])
    in case action of
        Tick dT ->
            updateTick dT model
        Upgrade ->
            no { model | attackDamage = model.attackDamage + 1 }

updateTick : Float -> Model -> (Model, List Currency.Bundle)
updateTick dT model =
    let updatedTimer =
            model.attackTimer + dT
        timeToAttack =
            1 / model.attackSpeed
        maxNumAttacks = 3
        numAttacks =
            updatedTimer / timeToAttack
                |> floor
                |> min maxNumAttacks
        updatedHealth = 
            model.health - numAttacks * model.attackDamage
        didDie =
            updatedHealth <= 0
    in ( { model
            | attackTimer =
                if numAttacks >= maxNumAttacks then
                    0
                else
                    updatedTimer - toFloat numAttacks * timeToAttack
            , health =
                if didDie then
                    model.maxHealth
                else
                    updatedHealth
            }
        ,   if didDie then
                [(Currency.Gold, model.goldReward)]
            else
                []
        )


view : Signal.Address (Currency.Bundle, Action) -> Model -> Html.Html
view address model =
    div []
        [ h3 [] [text "Battle"]
        , div [] [text <| "Health: " ++ Format.int model.health]
        , viewShop address model
        ]

viewShop : Signal.Address (Currency.Bundle, Action) -> Model -> Html.Html
viewShop address model =
    let cost =
            (Currency.Gold, (model.attackDamage - 9) * 4 + (model.attackDamage - 10) ^ 2)
    in div []
        [ span [] [text
            <| "Upgrade damage ("
            ++ toString model.attackDamage
            ++ ") :"
            ]
        , button
            [onClick address (cost, Upgrade)]
            [text <| Format.currency cost]
        ]