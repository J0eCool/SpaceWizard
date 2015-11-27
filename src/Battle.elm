module Battle where

import Html exposing (div, h3, text)

import Format

type alias Model =
    { health : Int
    , maxHealth : Int
    , attackTimer : Float
    , attackSpeed : Float
    , attackDamage : Int
    , goldReward : Int
    }

init : Model
init =
    { health = 100
    , maxHealth = 100
    , attackTimer = 0
    , attackSpeed = 1.2
    , attackDamage = 10
    , goldReward = 8
    }

update : Float -> Model -> Model
update dT model =
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
    in { model
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

view : Model -> Html.Html
view model =
    div []
        [ h3 [] [text "Battle"]
        , div [] [text <| "Health: " ++ Format.int model.health]
        ]
