module Battle where

import Color
import Html exposing (Html, div, h3, text, ul, li, button)
import Html.Events exposing (onClick)

import BattleStats exposing (attackDamage, attackSpeed, goldBonusMultiplier)
import Currency
import Format
import Widgets.ProgressBar as ProgressBar

type alias Model =
    { enemy : Enemy
    , highestLevelBeaten : Int
    , attackTimer : Float
    }

type alias Enemy =
    { level : Int
    , health : Int
    }

type Action
    = Tick Float
    | IncreaseLevel
    | DecreaseLevel

init : Model
init =
    { enemy =
        { level = 1
        , health = 50
        }
    , highestLevelBeaten = 0
    , attackTimer = 0
    }

update : Action -> BattleStats.Model -> Model -> (Model, List Currency.Bundle)
update action stats model =
    case action of
        Tick dT ->
            updateTick dT stats model
        IncreaseLevel ->
            updateEnemyLevel 1 model
        DecreaseLevel ->
            updateEnemyLevel (-1) model

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
        enemy = model.enemy
        updatedHealth =
            enemy.health - numAttacks * attackDamage stats
        didDie =
            updatedHealth <= 0
        updatedEnemy =
            { enemy
            | health =
                if didDie then
                    maxHealth model.enemy
                else
                    updatedHealth
            }
    in ( { model
            | attackTimer =
                if numAttacks >= maxNumAttacks then
                    0
                else
                    updatedTimer - toFloat numAttacks * timeToAttack
            , enemy = updatedEnemy
            , highestLevelBeaten =
                if didDie then
                    max model.highestLevelBeaten enemy.level
                else
                    model.highestLevelBeaten
            }
        ,   if didDie then
                reward stats model.enemy
            else
                []
        )

updateEnemyLevel : Int -> Model -> (Model, List Currency.Bundle)
updateEnemyLevel diff model =
    let enemy = model.enemy
        newLevel =
            enemy.level + diff
                |> clamp 1 (model.highestLevelBeaten + 1)
    in (if enemy.level == newLevel then
            model
        else
            { model
            | enemy =
                { enemy
                | level = newLevel
                }
                |> resetHealth
            , attackTimer = 0
            }
        , [])

view : Signal.Address Action -> BattleStats.Model -> Model -> Html
view address stats model =
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
        , viewLevel address model
        , div [] [text <| "Health: " ++ Format.int model.enemy.health]
        , ProgressBar.view healthBar
        , ProgressBar.view attackBar
        , div [] [text "Reward: "]
        , ul []
            <|  let currency = reward stats model.enemy
                    item c = li [] [text <| Format.currency c]
                in List.map item currency
        ]

viewLevel : Signal.Address Action -> Model -> Html
viewLevel address model =
    let levelButton action label =
            button [onClick address action] [text label]
        decButton =
            if model.enemy.level > 1 then
                [levelButton DecreaseLevel "-"]
            else
                []
        incButton =
            if model.enemy.level <= model.highestLevelBeaten then
                [levelButton IncreaseLevel "+"]
            else
                []
    in div []
        ([ text <| "Enemy level:"]
        ++ decButton
        ++ [text <| Format.int model.enemy.level]
        ++ incButton
        )

maxHealth : Enemy -> Int
maxHealth enemy =
    let l = enemy.level - 1
    in 50 + l * 9 + l ^ 2

resetHealth enemy =
    { enemy
    | health = maxHealth enemy
    }

reward : BattleStats.Model -> Enemy -> List Currency.Bundle
reward stats enemy =
    let baseGold = 5 + enemy.level
        baseExp = 9 + enemy.level ^ 2
    in
    [   ( Currency.Experience
        , baseExp
        )
    ,   ( Currency.Gold
        , round <| toFloat baseGold * goldBonusMultiplier stats
        )
    ]
