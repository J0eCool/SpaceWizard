module Battle where

import Html exposing (div, h3, text)
import Html.Events exposing (onClick)

import BattleStats exposing (attackDamage, attackSpeed)
import Currency
import Format

type alias Model =
    { health : Int
    , maxHealth : Int
    , attackTimer : Float
    , goldReward : Int
    }

type Action
    = Tick Float

init : Model
init =
    { health = 50
    , maxHealth = 50
    , attackTimer = 0
    , goldReward = 8
    }

update : Action -> BattleStats.Model -> Model -> (Model, List Currency.Bundle)
update action stats model =
    case action of
        Tick dT ->
            updateTick dT stats model

updateTick : Float -> BattleStats.Model -> Model -> (Model, List Currency.Bundle)
updateTick dT stats model =
    let updatedTimer =
            model.attackTimer + dT
        timeToAttack =
            1 / attackSpeed stats
        maxNumAttacks = 3
        numAttacks =
            updatedTimer / timeToAttack
                |> floor
                |> min maxNumAttacks
        updatedHealth = 
            model.health - numAttacks * attackDamage stats
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

view : Model -> Html.Html
view model =
    div []
        [ h3 [] [text "Battle"]
        , div [] [text <| "Health: " ++ Format.int model.health]
        ]
