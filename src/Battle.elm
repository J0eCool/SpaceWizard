module Battle where

import Color
import Html exposing (Html, div, span, h3, text, ul, li, button)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)

import BattleStats exposing (attackDamage, attackSpeed, goldBonusMultiplier)
import Currency
import Equipment
import Format
import Keys exposing (Key(..), Arrow(..))
import Operators exposing (..)
import Widgets
import Widgets.ProgressBar as ProgressBar

type alias Model =
  { player : Entity
  , enemy : Entity
  , enemyLevel : Int
  , highestLevelBeaten : Int
  , isPaused : Bool
  , respawnTimer : Float
  , autoSpawn : Bool
  , autoProgress : Bool
  , playerHadDied : Bool
  }

type alias Entity =
  { health : Int
  , partialHealth : Float
  , isDead : Bool
  , attackTimer : Float
  }

type Action
  = NoOp
  | Tick Float
  | KeyPress Keys.Key
  | SpawnEnemy
  | IncreaseLevel
  | DecreaseLevel
  | TogglePause
  | ToggleAutoSpawn
  | ToggleAutoProgress

type alias Context =
  { stats : BattleStats.Model
  , equip : Equipment.Model
  }
type alias Update =
  Context -> Model -> (Model, List Currency.Bundle)

init : Context -> Model
init ctx =
  { player = entityInit
  , enemy = entityInit
  , enemyLevel = 1
  , highestLevelBeaten = 0
  , isPaused = False
  , respawnTimer = 0
  , autoSpawn = True
  , autoProgress = False
  , playerHadDied = False
  }
  |> resetEnemyHealth
  |> resetPlayerHealth ctx

entityInit : Entity
entityInit =
  { health = 0
  , partialHealth = 0
  , isDead = False
  , attackTimer = 0
  }

update : Action -> Update
update action ctx model =
  let no m = (m, [])
  in case action of
    NoOp ->
      no model
    Tick dT ->
      if model.isPaused then
        no model
      else
        updateTick dT ctx model
    SpawnEnemy ->
      if not model.enemy.isDead then
        no model
      else
        updateDoRespawn model
    IncreaseLevel ->
      updateEnemyLevel 1 model
    DecreaseLevel ->
      updateEnemyLevel (-1) model
    TogglePause ->
      no { model | isPaused = not model.isPaused }
    ToggleAutoSpawn ->
      no { model | autoSpawn = not model.autoSpawn }
    ToggleAutoProgress ->
      no { model | autoProgress = not model.autoProgress }
    KeyPress key ->
      update (keyboardAction key) ctx model

keyboardAction : Key -> Action
keyboardAction key =
  case key of
    KeyArrow Right -> IncreaseLevel
    KeyArrow Left -> DecreaseLevel
    KeyChar ' ' -> TogglePause
    KeyChar 'a' -> ToggleAutoProgress
    KeyChar 's' -> SpawnEnemy
    _ -> NoOp

updateTick : Float -> Update
updateTick dT ctx model =
  let
    (regenModel, _) =
      updateRegen dT ctx model
  in
    updateRespawn dT ctx regenModel

updateRespawn : Float -> Update
updateRespawn dT ctx model =
  let
    isDead =
      model.enemy.isDead
    isRespawning =
      isDead && model.autoSpawn && not model.playerHadDied
    didRespawn =
      isRespawning && model.respawnTimer > timeToRespawn
    no m =
      (m, [])
  in
    if didRespawn then
      updateDoRespawn model
    else if isRespawning then
      no
        { model
        | respawnTimer = model.respawnTimer + dT
        }
    else if isDead then
      no model
    else
      updateDoAttacks dT ctx model

updateDoRespawn : Model -> (Model, List Currency.Bundle)
updateDoRespawn model =
  ( { model
    | respawnTimer = 0
    , enemy = { entityInit | health = maxEnemyHealth model }
    , playerHadDied = False
    }
  , [])

updateDoAttacks : Float -> Update
updateDoAttacks dT ctx model =
  let
    player =
      model.player
    playerStats =
      BattleStats.derived ctx.equip ctx.stats
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
      , reward ctx model)
    else if didPlayerDie then
      ( { model
        | enemy = { entityInit | isDead = True }
        , player = { updatedPlayer | isDead = False }
        , enemyLevel = max 1 <| model.enemyLevel - 1
        , autoProgress = False
        , playerHadDied = True
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
      , attackTimer =
          if didDie then 0 else target.attackTimer
      , isDead =
          didDie
      }
    updatedAttacker =
      { attacker
      | attackTimer =
          updatedAttackTimer
      }
  in
    (updatedAttacker, updatedTarget, didDie)

updateRegen : Float -> Update
updateRegen dT ctx model =
  let
    player =
      model.player
    playerStats =
      BattleStats.derived ctx.equip ctx.stats
    enemy =
      model.enemy
    enemyStats =
      enemyDerived model
    regen stat ent =
      let
        dPartial =
          if not ent.isDead then
            stat.healthRegen * dT
          else
            0
        tickedPartial =
          ent.partialHealth + dPartial
        dHealth =
          floor tickedPartial
        updatedHealth =
          ent.health + dHealth
            |> min stat.maxHealth
        updatedPartial =
          tickedPartial - toFloat dHealth
      in
        { ent
        | health = updatedHealth
        , partialHealth = updatedPartial
        }
    updatedHadDied =
      model.playerHadDied && player.health < playerStats.maxHealth
  in
    ( { model
      | enemy = regen enemyStats enemy
      , player = regen playerStats player
      , playerHadDied = updatedHadDied
      }
    , [])

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
    if model.enemyLevel == newLevel then
      (model, [])
    else
      { model
      | enemyLevel = newLevel
      , player = updatedPlayer
      }
      |> updateDoRespawn

view : Signal.Address Action -> Context -> Model -> Html
view address ctx model =
  let
    spawnTime =
      timeToRespawn - model.respawnTimer
    spawnButton =
      div []
        [button
          [ onClick address SpawnEnemy
          , disabled <| model.enemy.health > 0
          ]
          [text <| "Spawn (" ++ Format.floatWithDigits 1 spawnTime ++ "s)"]
        ]
  in div []
    [ h3 [] [text "Battle"]
    , viewLevel address model
    , viewEntity "Player" True (BattleStats.derived ctx.equip ctx.stats) model.player
    , viewEntity "Enemy" False (enemyDerived model) model.enemy
    , div [] [text "Reward: "]
    , viewRewards ctx model
    , spawnButton
    , div []
        [ Widgets.checkbox address "Pause" TogglePause model.isPaused
        , Widgets.checkbox address "Auto-spawn" ToggleAutoSpawn model.autoSpawn
        , Widgets.checkbox address "Auto-progress" ToggleAutoProgress model.autoProgress
        ]
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

viewRewards : Context -> Model -> Html
viewRewards ctx model =
  let
    currency =
      reward ctx model
    damage =
      attackDamage ctx.equip ctx.stats - enemyArmor model
    attacksToKill =
      ceiling <| maxEnemyHealth model ./ damage
    timePerKill =
      timeToRespawn + toFloat attacksToKill / attackSpeed ctx.equip ctx.stats
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

enemyAttackDamage : Model -> Int
enemyAttackDamage model =
  let l = model.enemyLevel - 1
  in 10 + 3 * l + l ^ 2

enemyArmor : Model -> Int
enemyArmor model =
  let l = toFloat <| model.enemyLevel - 1
  in floor <| l * 4 + 0.5 * l ^ 1.5

enemyDerived : Model -> BattleStats.Derived
enemyDerived model =
  { maxHealth = maxEnemyHealth model
  , healthRegen = 2
  , attackDamage = enemyAttackDamage model
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

resetPlayerHealth : Context -> Model -> Model
resetPlayerHealth ctx model =
  let
    player =
      model.player
    updatedPlayer =
      { player
      | health = BattleStats.maxHealth ctx.stats
      }
  in { model | player = updatedPlayer }

timeToRespawn : Float
timeToRespawn =
  4

reward : Context -> Model -> List Currency.Bundle
reward ctx model =
  let
    l = model.enemyLevel
    baseGold = 5 + 2 * l + floor ((toFloat l) ^ 1.5)
    baseExp = 6 + 3 * l + l ^ 2
  in
  [ ( Currency.Experience
    , baseExp
    )
  , ( Currency.Gold
    , round <| toFloat baseGold * goldBonusMultiplier ctx.stats
    )
  ]
