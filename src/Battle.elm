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
  { player = entityInit
  , enemy = entityInit
  , enemyLevel = 1
  , highestLevelBeaten = 0
  , isAttacking = True
  , respawnTimer = 0
  , autoProgress = False
  }
  |> resetEnemyHealth
  |> resetPlayerHealth stats

entityInit : Entity
entityInit =
  { health = 0
  , attackTimer = 0
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
    KeyChar 'a' -> ToggleAutoProgress
    _ -> NoOp

updateTick : Float -> BattleStats.Model -> Model -> (Model, List Currency.Bundle)
updateTick dT stats model =
  let
    isRespawning =
      model.enemy.health <= 0
    didRespawn =
      isRespawning && model.respawnTimer > timeToRespawn
    canAttack =
      model.isAttacking && not isRespawning
    no m =
      (m, [])
  in
    if didRespawn then
      let enemy = model.enemy
      in no
        { model
        | respawnTimer = 0
        , enemy = { enemy | health = maxEnemyHealth model }
        }
    else if isRespawning then
      no
        { model
        | respawnTimer = model.respawnTimer + dT
        }
    else if canAttack then
      updateAttacks dT stats model
    else
      no model

updateAttacks : Float -> BattleStats.Model -> Model -> (Model, List Currency.Bundle)
updateAttacks dT stats model =
  let
    player =
      model.player
    playerStats =
      BattleStats.derived stats
    enemy =
      model.enemy
    enemyStats =
      enemyDerived model
    (tempPlayer, tempEnemy, didEnemyDie) =
      updateAttacker dT playerStats player enemyStats enemy
    (updatedEnemy, updatedPlayer, didPlayerDie) =
      updateAttacker dT enemyStats tempEnemy playerStats tempPlayer
  in
    if didEnemyDie then
      ( { model
        | enemy = tempEnemy
        , player = tempPlayer
        , highestLevelBeaten =
            max model.highestLevelBeaten model.enemyLevel
        , enemyLevel =
            model.enemyLevel + (if model.autoProgress then 1 else 0)
        }
      , reward stats model)
    else if didPlayerDie then
      ( { model
        | enemy = entityInit
        , player = updatedPlayer
        , enemyLevel = 1
        , highestLevelBeaten = 0
        }
      , [])
    else
      ( { model
        | enemy = updatedEnemy
        , player = updatedPlayer
        }
      , [])

updateAttacker : Float ->
  BattleStats.Derived -> Entity ->
  BattleStats.Derived -> Entity ->
  (Entity, Entity, Bool)
updateAttacker dT attackStats attacker targetStats target =
  let
    attackTimer =
      attacker.attackTimer + dT
    timeToAttack =
      1 / attackStats.attackSpeed
    maxNumAttacks =
      3
    numAttacks =
      attackTimer / timeToAttack
        |> floor
        |> min maxNumAttacks
    updatedAttackTimer =
      if numAttacks >= maxNumAttacks then
        0
      else
        attackTimer - toFloat numAttacks * timeToAttack
    damage =
      attackStats.attackDamage - targetStats.armor
        |> max 0
    updatedHealth =
      target.health - numAttacks * damage
        |> max 0
    didDie =
      updatedHealth <= 0
    updatedTarget =
      { target
      | health =
          updatedHealth
      }
    updatedAttacker =
      { attacker
      | attackTimer =
          updatedAttackTimer
      }
  in
    (updatedAttacker, updatedTarget, didDie)

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
    , viewEntity "Player" True (BattleStats.derived stats) model.player
    , viewEntity "Enemy" False (enemyDerived model) model.enemy
    , div [] 
      [checkbox address "Attack" ToggleAttack model.isAttacking]
    , div [] 
      [checkbox address "Auto-progress" ToggleAutoProgress model.autoProgress]
    , div [] [text "Reward: "]
    , viewRewards stats model
    ]

viewEntity : String -> Bool -> BattleStats.Derived -> Entity -> Html
viewEntity title isPlayer stats entity =
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
      healthStr =
        Format.int entity.health
        ++ " / "
        ++ Format.int stats.maxHealth
      enemyStats =
        if isPlayer then
          []
        else
          [ div [] [text <| "Damage: " ++ Format.int stats.attackDamage]
          , div [] [text <| "Armor: " ++ Format.int stats.armor]
          , div [] [text <| "Health Regen: " ++ Format.float stats.healthRegen ++ "/s"]
          ]
    in div [] (
      [ div [] [text title]
      , ProgressBar.view healthBar
      , ProgressBar.view attackBar
      , div [] [text <| "Health: " ++ healthStr]
      ]
      ++ enemyStats
      )

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
  , healthRegen = 2
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
