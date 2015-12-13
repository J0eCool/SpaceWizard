module Battle where

import Color
import Html exposing (Html, div, h3, text, ul, li, button, input)
import Html.Attributes exposing (type', checked)
import Html.Events exposing (onClick)

import BattleStats exposing (attackDamage, attackSpeed, goldBonusMultiplier)
import Currency
import Format
import Keys exposing (Key(..), Arrow(..))
import Operators exposing (..)
import Widgets.ProgressBar as ProgressBar

type alias Model =
    { enemy : Enemy
    , highestLevelBeaten : Int
    , attackTimer : Float
    , isAttacking : Bool
    , respawnTimer : Float
    , autoProgress : Bool
    }

type alias Enemy =
    { level : Int
    , health : Int
    }

type Action
    = NoOp
    | Tick Float
    | KeyPress Keys.Key
    | IncreaseLevel
    | DecreaseLevel
    | ToggleAttack
    | ToggleAutoProgress

init : Model
init =
    let enemy = 
        { level = 1
        , health = 0
        } 
    in  { enemy = { enemy | health = maxHealth enemy }
        , highestLevelBeaten = 0
        , attackTimer = 0
        , isAttacking = True
        , respawnTimer = 0
        , autoProgress = False
        }

update : Action -> BattleStats.Model -> Model -> (Model, List Currency.Bundle)
update action stats model =
    let no m = (m, [])
    in case action of
        NoOp ->
            no model
        Tick dT ->
            updateTick dT stats model
        IncreaseLevel ->
            updateEnemyLevel 1 model
        DecreaseLevel ->
            updateEnemyLevel (-1) model
        ToggleAttack ->
            no { model | isAttacking = not model.isAttacking }
        ToggleAutoProgress ->
            no { model | autoProgress = not model.autoProgress }
        KeyPress key ->
            update (keyboardAction key) stats model

keyboardAction : Key -> Action
keyboardAction key =
    case key of
        KeyArrow Right -> IncreaseLevel
        KeyArrow Left -> DecreaseLevel
        KeyChar ' ' -> ToggleAttack
        _ -> NoOp

updateTick : Float -> BattleStats.Model -> Model -> (Model, List Currency.Bundle)
updateTick dT stats model =
    let isRespawning =
            model.enemy.health <= 0
        canAttack =
            model.isAttacking && not isRespawning
        dIf cond =
            if cond then dT else 0
        attackTimer =
            model.attackTimer + dIf canAttack
        respawnTimer =
            model.respawnTimer + dIf isRespawning
        timeToAttack =
            1 / attackSpeed stats
        maxNumAttacks =
            3
        numAttacks =
            attackTimer / timeToAttack
                |> floor
                |> min maxNumAttacks
        enemy =
            model.enemy
        damage =
            attackDamage stats - armor enemy
                |> max 0
        updatedHealth =
            enemy.health - numAttacks * damage
                |> max 0
        didDie =
            not isRespawning && updatedHealth <= 0
        didRespawn =
            isRespawning && respawnTimer > timeToRespawn
        highestLevelBeaten =
            if didDie then
                max model.highestLevelBeaten enemy.level
            else
                model.highestLevelBeaten
        level =
            if model.autoProgress && didDie then
                highestLevelBeaten + 1
            else
                enemy.level
        updatedEnemy =
            { enemy
            | health =
                if didRespawn then
                    maxHealth model.enemy
                else
                    updatedHealth
            , level = level
            }
    in ( { model
            | attackTimer =
                if numAttacks >= maxNumAttacks || isRespawning then
                    0
                else
                    attackTimer - toFloat numAttacks * timeToAttack
            , respawnTimer =
                if didRespawn then
                    0
                else
                    respawnTimer
            , enemy =
                updatedEnemy
            , highestLevelBeaten =
                highestLevelBeaten
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
                |>  if enemy.health > 0 then
                        resetHealth
                    else
                        identity
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
        attackText =
            if model.isAttacking then
                "Pause"
            else
                "Attack"
        attackButton =
            button [onClick address ToggleAttack] [text attackText]
    in div []
        [ h3 [] [text "Battle"]
        , viewLevel address model
        , div [] [text <| "Health: " ++ Format.int model.enemy.health]
        , div [] [text <| "Amor: " ++ Format.int (armor model.enemy)]
        , ProgressBar.view healthBar
        , ProgressBar.view attackBar
        , div [] 
            [checkbox address "Attack" ToggleAttack model.isAttacking]
        , div [] 
            [checkbox address "Auto-progress" ToggleAutoProgress model.autoProgress]
        , div [] [text "Reward: "]
        , viewRewards stats model
        ]

checkbox : Signal.Address Action -> String -> Action -> Bool -> Html
checkbox address label action value =
    div []
        [ text <| label ++ ": "
        , input
            [ type' "checkbox"
            , checked value
            , onClick address action
            ]
            []
        ]

viewLevel : Signal.Address Action -> Model -> Html
viewLevel address model =
    let levelButton action label cond =
            if cond then
                [button [onClick address action] [text label]]
            else
                []
        decButton =
            levelButton DecreaseLevel "-"
                <| model.enemy.level > 1
        incButton =
            levelButton IncreaseLevel "+"
                <| model.enemy.level <= model.highestLevelBeaten
    in div []
        ( [ text <| "Enemy level:"]
        ++ decButton
        ++ [text <| Format.int model.enemy.level]
        ++ incButton
        )

viewRewards : BattleStats.Model -> Model -> Html
viewRewards stats model =
    let currency =
            reward stats model.enemy
        damage =
            attackDamage stats - armor model.enemy
        attacksToKill =
            ceiling <| maxHealth model.enemy ./ damage
        timePerKill =
            timeToRespawn + toFloat attacksToKill / attackSpeed stats
        perSecond =
            Currency.bundleMap (\cur -> toFloat cur / timePerKill)
        item c =
            li []
                [ text <| Format.currency c
                    ++ " (+"
                    ++ Format.floatCurrency (perSecond c)
                    ++ "/s)"
                ]
    in ul [] <| List.map item currency

maxHealth : Enemy -> Int
maxHealth enemy =
    let l = enemy.level - 1
    in 100 + 18 * l + 2 * l ^ 2

armor : Enemy -> Int
armor enemy =
    let l = toFloat <| enemy.level - 1
    in floor <| l * 4 + 0.5 * l ^ 1.5

resetHealth : Enemy -> Enemy
resetHealth enemy =
    { enemy
    | health = maxHealth enemy
    }

timeToRespawn : Float
timeToRespawn =
    1

reward : BattleStats.Model -> Enemy -> List Currency.Bundle
reward stats enemy =
    let l = enemy.level
        baseGold = 5 + 2 * l + floor ((toFloat l) ^ 1.5)
        baseExp = 6 + 3 * l + l ^ 2
    in
    [   ( Currency.Experience
        , baseExp
        )
    ,   ( Currency.Gold
        , round <| toFloat baseGold * goldBonusMultiplier stats
        )
    ]
