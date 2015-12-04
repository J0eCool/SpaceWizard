module Battle where

import Color
import Html exposing (Html, div, h3, text, ul, li)
import Html.Events exposing (onClick)

import BattleStats exposing (attackDamage, attackSpeed, goldBonusMultiplier)
import Currency
import Format
import Widgets.ProgressBar as ProgressBar

type alias Model =
    { enemy : Enemy
    , attackTimer : Float
    , gold : Int
    , experience : Int
    }

type alias Enemy =
    { level : Int
    , health : Int
    }

type Action
    = Tick Float

init : Model
init =
    { enemy =
        { level = 1
        , health = 50
        }
    , attackTimer = 0
    , gold = 8
    , experience = 1
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
        updateEnemy enemy =
            let updatedHealth = 
                    enemy.health - numAttacks * attackDamage stats
                didDie =
                    updatedHealth <= 0
            in ({ enemy
                | health =
                    if didDie then
                        maxHealth model.enemy
                    else
                        updatedHealth
                }
                , didDie
                )
        (updatedEnemy, didDie) = updateEnemy model.enemy
    in ( { model
            | attackTimer =
                if numAttacks >= maxNumAttacks then
                    0
                else
                    updatedTimer - toFloat numAttacks * timeToAttack
            , enemy = updatedEnemy
            }
        ,   if didDie then
                reward stats model.enemy
            else
                []
        )

view : BattleStats.Model -> Model -> Html
view stats model =
    let healthBar =
            { width = 300
            , height = 20
            , curAmount = toFloat <| model.enemy.health
            , maxAmount = toFloat <| maxHealth model.enemy
            , color = Color.rgb 240 32 32
            , background = Color.rgb 128 16 16
            }
        attackBar =
            { width = 300
            , height = 12
            , curAmount = model.attackTimer
            , maxAmount = 1 / attackSpeed stats
            , color = Color.rgb 255 0 255
            , background = Color.rgb 32 32 32
            }
    in div []
        [ h3 [] [text "Battle"]
        , div [] [text <| "Enemy level:" ++ Format.int model.enemy.level]
        , div [] [text <| "Health: " ++ Format.int model.enemy.health]
        , ProgressBar.view healthBar
        , ProgressBar.view attackBar
        , div [] [text "Reward: "]
        , ul []
            <|  let currency = reward stats model.enemy
                    item c = li [] [text <| Format.currency c]
                in List.map item currency
        ]

maxHealth : Enemy -> Int
maxHealth enemy =
    let l = enemy.level - 1
    in 50 + l * 10 + l ^ 2

reward : BattleStats.Model -> Enemy -> List Currency.Bundle
reward stats enemy =
    let baseGold = 5 + enemy.level
        baseExp = 9 + enemy.level ^ 2
    in
    [   ( Currency.Gold
        , round <| toFloat baseGold * goldBonusMultiplier stats
        )
    ,   ( Currency.Experience
        , baseExp
        )
    ]
