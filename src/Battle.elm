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
  { player : Entity
  , enemy : Entity
  , enemyLevel : Int
  , highestLevelBeaten : Int
  , isAttacking : Bool
  , respawnTimer : Float
  , autoProgress : Bool
  }

type alias Entity =
  { health : Int
  , attackTimer : Float
  }

type Action
  = NoOp
  | Tick Float
  | KeyPress Keys.Key
  | IncreaseLevel
  | DecreaseLevel
  | ToggleAttack
  | ToggleAutoProgress

init : BattleStats.Model -> Model
init stats =
  let
    enemy = 
      { health = 0
      , attackTimer = 0
      }
  in
    { player = enemy
    , enemy = enemy
    , enemyLevel = 1
    , highestLevelBeaten = 0
    , isAttacking = True
    , respawnTimer = 0
    , autoProgress = False
    }
    |> resetEnemyHealth
    |> resetPlayerHealth stats

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
    KeyChar 'a' -> ToggleAutoProgress
    _ -> NoOp

updateTick : Float -> BattleStats.Model -> Model -> (Model, List Currency.Bundle)
updateTick dT stats model =
  let
    isRespawning =
      model.enemy.health <= 0
    canAttack =
      model.isAttacking && not isRespawning
    dIf cond =
      if cond then dT else 0
    attackTimer =
      model.player.attackTimer + dIf canAttack
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
    updatedAttackTimer =
      if numAttacks >= maxNumAttacks || isRespawning then
        0
      else
        attackTimer - toFloat numAttacks * timeToAttack
    player =
      model.player
    enemy =
      model.enemy
    damage =
      attackDamage stats - enemyArmor model
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
        max model.highestLevelBeaten model.enemyLevel
      else
        model.highestLevelBeaten
    level =
      if model.autoProgress && didDie then
        highestLevelBeaten + 1
      else
        model.enemyLevel
    updatedEnemy =
      { enemy
      | health =
        if didRespawn then
          maxEnemyHealth model
        else
          updatedHealth
      }
    updatedPlayer =
      { player
      | attackTimer = updatedAttackTimer
      }
  in ( { model
      | respawnTimer =
          if didRespawn then
            0
          else
            respawnTimer
      , enemy =
          updatedEnemy
      , enemyLevel =
          level
      , player =
          updatedPlayer
      , highestLevelBeaten =
          highestLevelBeaten
      }
    ,   if didDie then
        reward stats model
      else
        []
    )

updateEnemyLevel : Int -> Model -> (Model, List Currency.Bundle)
updateEnemyLevel diff model =
  let
    newLevel =
      model.enemyLevel + diff
        |> clamp 1 (model.highestLevelBeaten + 1)
    player =
      model.player
    updatedPlayer =
      { player
      | attackTimer = 0
      }
  in
    ( if model.enemyLevel == newLevel then
        model
      else
        { model
        | enemyLevel = newLevel
        , player = updatedPlayer
        }
        |>
          if model.enemy.health > 0 then
            resetEnemyHealth
          else
            identity
    , [])

view : Signal.Address Action -> BattleStats.Model -> Model -> Html
view address stats model =
  let
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
    , viewEntity "Player" (BattleStats.derived stats) model.player
    , viewEntity "Enemy" (enemyDerived model) model.enemy
    , div [] 
      [checkbox address "Attack" ToggleAttack model.isAttacking]
    , div [] 
      [checkbox address "Auto-progress" ToggleAutoProgress model.autoProgress]
    , div [] [text "Reward: "]
    , viewRewards stats model
    ]

viewEntity : String -> BattleStats.Derived -> Entity -> Html
viewEntity title stats entity =
    let
      healthBar =
        { width = 300
        , height = 20
        , curAmount = toFloat <| entity.health
        , maxAmount = toFloat <| stats.maxHealth
        , color = Color.rgb 240 32 32
        , background = Color.rgb 128 16 16
        }
      attackBar =
        { width = 300
        , height = 12
        , curAmount = entity.attackTimer
        , maxAmount = 1 / stats.attackSpeed
        , color = Color.rgb 255 0 255
        , background = Color.rgb 32 32 32
        }
    in div []
      [ div [] [text title]
      , div [] [text <| "Health: " ++ Format.int entity.health]
      , div [] [text <| "Damage: " ++ Format.int stats.attackDamage]
      , div [] [text <| "Amor: " ++ Format.int stats.armor]
      , ProgressBar.view healthBar
      , ProgressBar.view attackBar
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
  let
    levelButton action label cond =
      if cond then
        [button [onClick address action] [text label]]
      else
        []
    decButton =
      levelButton DecreaseLevel "-"
        <| model.enemyLevel > 1
    incButton =
      levelButton IncreaseLevel "+"
        <| model.enemyLevel <= model.highestLevelBeaten
  in div []
    ( [ text <| "Enemy level:"]
    ++ decButton
    ++ [text <| Format.int model.enemyLevel]
    ++ incButton
    )

viewRewards : BattleStats.Model -> Model -> Html
viewRewards stats model =
  let
    currency =
      reward stats model
    damage =
      attackDamage stats - enemyArmor model
    attacksToKill =
      ceiling <| maxEnemyHealth model ./ damage
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

maxEnemyHealth : Model -> Int
maxEnemyHealth model =
  let l = model.enemyLevel - 1
  in 100 + 18 * l + 2 * l ^ 2

enemyArmor : Model -> Int
enemyArmor model =
  let l = toFloat <| model.enemyLevel - 1
  in floor <| l * 4 + 0.5 * l ^ 1.5

enemyDerived : Model -> BattleStats.Derived
enemyDerived model =
  { maxHealth = maxEnemyHealth model
  , attackDamage = 5
  , attackSpeed = 1
  , armor = enemyArmor model
  }

resetEnemyHealth : Model -> Model
resetEnemyHealth model =
  let
    enemy =
      model.enemy
    updatedEnemy =
      { enemy
      | health = maxEnemyHealth model
      }
  in { model | enemy = updatedEnemy }

resetPlayerHealth : BattleStats.Model -> Model -> Model
resetPlayerHealth stats model =
  let
    player =
      model.player
    updatedPlayer =
      { player
      | health = BattleStats.maxHealth stats
      }
  in { model | player = updatedPlayer }

timeToRespawn : Float
timeToRespawn =
  1

reward : BattleStats.Model -> Model -> List Currency.Bundle
reward stats model =
  let
    l = model.enemyLevel
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
