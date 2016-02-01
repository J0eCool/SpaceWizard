module Battle (..) where

import Color
import Html exposing (Html, div, span, h3, text, ul, li, button, img)
import Html.Attributes exposing (disabled, src)
import Html.Events exposing (onClick)
import BattleStats exposing (attackDamage, attackSpeed, goldBonusMultiplier)
import Currency
import Equipment
import Format
import Keys exposing (Key(..), Arrow(..))
import Map
import Operators exposing (..)
import Style exposing (..)
import Widgets
import Widgets.ProgressBar as ProgressBar


type alias Model =
  { player : Entity
  , enemy : Entity
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
  | TogglePause
  | ToggleAutoSpawn
  | ToggleAutoProgress
  | ChangedMap
  | MapAction Map.Action


type alias Context r =
  { r
    | stats : BattleStats.Model
    , equipment : Equipment.Model
    , map : Map.Model
  }


type alias Output =
  { rewards : List Currency.Bundle
  , mapAction : Maybe Map.Action
  }


type alias Update a =
  Context a -> Model -> ( Model, Output )


init : Context a -> Model
init ctx =
  { player = entityInit
  , enemy = entityInit
  , isPaused = False
  , respawnTimer = 0
  , autoSpawn = True
  , autoProgress = False
  , playerHadDied = False
  }
    |> resetEnemyHealth ctx
    |> resetPlayerHealth ctx


entityInit : Entity
entityInit =
  { health = 0
  , partialHealth = 0
  , isDead = False
  , attackTimer = 0
  }


no : Model -> ( Model, Output )
no model =
  ( model, { rewards = [], mapAction = Nothing } )


update : Action -> Update a
update action ctx model =
  case action of
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
        updateDoRespawn ctx model

    TogglePause ->
      no { model | isPaused = not model.isPaused }

    ToggleAutoSpawn ->
      no { model | autoSpawn = not model.autoSpawn }

    ToggleAutoProgress ->
      no { model | autoProgress = not model.autoProgress }

    KeyPress key ->
      update (keyboardAction key) ctx model

    ChangedMap ->
      updateDoRespawn ctx model

    MapAction action ->
      ( model, { rewards = [], mapAction = Just action } )


keyboardAction : Key -> Action
keyboardAction key =
  case key of
    KeyChar ' ' ->
      TogglePause

    KeyChar 'a' ->
      ToggleAutoProgress

    KeyChar 's' ->
      SpawnEnemy

    _ ->
      NoOp


updateTick : Float -> Update a
updateTick dT ctx model =
  let
    ( regenModel, _ ) =
      updateRegen dT ctx model
  in
    updateRespawn dT ctx regenModel


updateRespawn : Float -> Update a
updateRespawn dT ctx model =
  let
    isDead =
      model.enemy.isDead

    isRespawning =
      isDead && model.autoSpawn && not model.playerHadDied

    didRespawn =
      isRespawning && model.respawnTimer > timeToRespawn
  in
    if didRespawn then
      updateDoRespawn ctx model
    else if isRespawning then
      no
        { model
          | respawnTimer = model.respawnTimer + dT
        }
    else if isDead then
      no model
    else
      updateDoAttacks dT ctx model


updateDoRespawn : Update a
updateDoRespawn ctx model =
  let
    player =
      model.player

    updatedPlayer =
      { player
        | attackTimer = 0
      }
  in
    { model
      | respawnTimer = 0
      , player = updatedPlayer
      , playerHadDied = False
    }
      |> resetEnemyHealth ctx
      |> no


updateDoAttacks : Float -> Update a
updateDoAttacks dT ctx model =
  let
    player =
      model.player

    playerStats =
      BattleStats.derived ctx.equipment ctx.stats

    enemy =
      model.enemy

    enemyStats =
      Map.enemyDerived ctx.map

    ( tempPlayer, tempEnemy, didEnemyDie ) =
      updateAttacker dT playerStats player enemyStats enemy

    ( updatedEnemy, updatedPlayer, didPlayerDie ) =
      updateAttacker dT enemyStats tempEnemy playerStats tempPlayer
  in
    if didEnemyDie then
      ( { model
          | enemy = tempEnemy
          , player =
              tempPlayer
              -- TODO: re-enable auto-progress by sending multiple actions
              --, enemyLevel =
              --    model.enemyLevel + (if model.autoProgress then 1 else 0)
        }
      , { rewards = reward ctx
        , mapAction = Just Map.BeatStage
        }
      )
    else if didPlayerDie then
      ( { model
          | enemy = { entityInit | isDead = True }
          , player = { updatedPlayer | isDead = False }
          , autoProgress = False
          , playerHadDied = True
        }
      , { rewards = []
        , mapAction = Just Map.Decrement
        }
      )
    else
      { model
        | enemy = updatedEnemy
        , player = updatedPlayer
      }
        |> no


updateAttacker : Float -> BattleStats.Derived -> Entity -> BattleStats.Derived -> Entity -> ( Entity, Entity, Bool )
updateAttacker dT attackStats attacker targetStats target =
  let
    attackTimer =
      attacker.attackTimer + dT

    timeToAttack =
      1 / attackStats.attackSpeed

    maxNumAttacks =
      3

    numAttacks =
      attackTimer
        / timeToAttack
        |> floor
        |> min maxNumAttacks

    updatedAttackTimer =
      if numAttacks >= maxNumAttacks then
        0
      else
        attackTimer - toFloat numAttacks * timeToAttack

    damage =
      attackStats.attackDamage
        - targetStats.armor
        |> max 0

    updatedHealth =
      target.health
        - numAttacks
        * damage
        |> max 0

    didDie =
      updatedHealth <= 0

    updatedTarget =
      { target
        | health =
            updatedHealth
        , attackTimer =
            if didDie then
              0
            else
              target.attackTimer
        , isDead =
            didDie
      }

    updatedAttacker =
      { attacker
        | attackTimer =
            updatedAttackTimer
      }
  in
    ( updatedAttacker, updatedTarget, didDie )


updateRegen : Float -> Update a
updateRegen dT ctx model =
  let
    player =
      model.player

    playerStats =
      BattleStats.derived ctx.equipment ctx.stats

    enemy =
      model.enemy

    enemyStats =
      Map.enemyDerived ctx.map

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
          ent.health
            + dHealth
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
    { model
      | enemy = regen enemyStats enemy
      , player = regen playerStats player
      , playerHadDied = updatedHadDied
    }
      |> no


view : Signal.Address Action -> Context a -> Model -> Html
view address ctx model =
  let
    spawnTime =
      timeToRespawn - model.respawnTimer

    spawnButton =
      div
        []
        [ button
            [ onClick address SpawnEnemy
            , disabled <| model.enemy.health > 0
            ]
            [ text <| "Spawn (" ++ Format.floatWithDigits 1 spawnTime ++ "s)" ]
        ]
  in
    div
      []
      [ h3 [] [ text "Battle" ]
      , viewLevel address ctx
      , viewEntity True (BattleStats.derived ctx.equipment ctx.stats) model.player
      , viewEntity False (Map.enemyDerived ctx.map) model.enemy
      , div [] [ text "Reward: " ]
      , viewRewards ctx model
      , spawnButton
      , div
          []
          [ Widgets.checkbox address "Pause" TogglePause model.isPaused
          , Widgets.checkbox address "Auto-spawn" ToggleAutoSpawn model.autoSpawn
          , Widgets.checkbox address "Auto-progress" ToggleAutoProgress model.autoProgress
          ]
      ]


viewEntity : Bool -> BattleStats.Derived -> Entity -> Html
viewEntity isPlayer stats entity =
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
        [ div [] [ text <| "Damage: " ++ Format.int stats.attackDamage ]
        , div [] [ text <| "Armor: " ++ Format.int stats.armor ]
        , div [] [ text <| "Health Regen: " ++ Format.float stats.healthRegen ++ "/s" ]
        ]

    image =
      img
        [ style
            ([ display InlineBlock
             , width (Px 100)
             , height (Px 100)
             , verticalAlign Top
             , pixelated
             ]
              ++ if isPlayer then
                  []
                 else
                  [ flipHorizontal ]
            )
        , src stats.image
        ]
        []

    ( left, right ) =
      if isPlayer then
        ( [ image ], [] )
      else
        ( [], [ image ] )
  in
    div
      []
      (left
        ++ [ div
              [ style [ display InlineBlock ] ]
              ([ div [] [ text <| stats.name ++ " (Pow " ++ Format.int (BattleStats.power stats) ++ ")" ]
               , ProgressBar.view healthBar
               , ProgressBar.view attackBar
               , div [] [ text <| "Health: " ++ healthStr ]
               ]
                ++ enemyStats
              )
           ]
        ++ right
      )


viewLevel : Signal.Address Action -> Context a -> Html
viewLevel address ctx =
  let
    levelButton action label cond =
      if cond then
        [ button [ onClick address action ] [ text label ] ]
      else
        []

    area =
      Map.selected ctx.map

    decButton =
      levelButton (MapAction Map.Decrement) "-"
        <| area.stage
        > 1

    incButton =
      levelButton (MapAction Map.Increment) "+"
        <| area.stage
        <= area.highestStageBeaten
  in
    div
      []
      [ div [] [ text area.name ]
      , div
          []
          ([ text <| "Stage:" ]
            ++ decButton
            ++ [ text <| Format.int <| .stage <| Map.selected ctx.map ]
            ++ incButton
          )
      ]


viewRewards : Context a -> Model -> Html
viewRewards ctx model =
  let
    currency =
      reward ctx

    enemyStats =
      Map.enemyDerived ctx.map

    damage =
      attackDamage ctx.equipment ctx.stats - enemyStats.armor

    attacksToKill =
      ceiling <| enemyStats.maxHealth ./ damage

    timePerKill =
      timeToRespawn + toFloat attacksToKill / attackSpeed ctx.equipment ctx.stats

    perSecond =
      Currency.bundleMap (\cur -> toFloat cur / timePerKill)

    item c =
      li
        []
        [ text
            <| Format.currency c
            ++ " (+"
            ++ Format.floatCurrency (perSecond c)
            ++ "/s)"
        ]
  in
    ul [] <| List.map item currency


resetEnemyHealth : Context a -> Model -> Model
resetEnemyHealth ctx model =
  let
    enemy =
      model.enemy

    enemyStats =
      Map.enemyDerived ctx.map

    updatedEnemy =
      { enemy
        | health = enemyStats.maxHealth
        , isDead = False
      }
  in
    { model | enemy = updatedEnemy }


resetPlayerHealth : Context a -> Model -> Model
resetPlayerHealth ctx model =
  let
    player =
      model.player

    updatedPlayer =
      { player
        | health = BattleStats.maxHealth ctx.stats
      }
  in
    { model | player = updatedPlayer }


timeToRespawn : Float
timeToRespawn =
  2.5


reward : Context a -> List Currency.Bundle
reward ctx =
  let
    l =
      Map.enemyLevel ctx.map

    baseGold =
      5 + 2 * l + floor ((toFloat l) ^ 1.5)

    baseExp =
      6 + 3 * l + l ^ 2
  in
    [ ( Currency.Experience
      , baseExp
      )
    , ( Currency.Gold
      , round <| toFloat baseGold * goldBonusMultiplier ctx.stats
      )
    ]
