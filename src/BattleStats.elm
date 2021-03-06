module BattleStats (..) where

import Color
import Focus exposing (..)
import Html exposing (Html, div, h3, text, span, button, ul, li)
import Html.Events exposing (onMouseDown, onMouseUp, onMouseEnter, onMouseLeave)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Cost
import Currency
import Equipment
import Format
import ListUtil exposing (mapSum)
import Serialize
import Style exposing (..)
import Widgets.ProgressBar as ProgressBar
import Widgets.UpgradeSlot as UpgradeSlot


type alias Model =
  { strength : Stat
  , speed : Stat
  , vitality : Stat
  , endurance : Stat
  , luck : Stat
  , heldAction : Maybe TimedAction
  , hoveredUpgrade : Maybe TimedAction
  , upgradeVelocity : Float
  }


type alias Derived =
  { name : String
  , image : String
  , maxHealth : Int
  , healthRegen : Float
  , attackDamage : Int
  , attackSpeed : Float
  , armor : Int
  }


type alias Stat =
  { level : Float
  , name : String
  }


type Action
  = UpgradeAction (UpgradeSlot.Action TimedAction)
  | Tick Float


type TimedAction
  = Upgrade (Focus Model Stat)
  | LimitedUpgrade Int (Focus Model Stat)


init : Model
init =
  { strength =
      { initStat | name = "Strength" }
  , speed =
      { initStat | name = "Speed" }
  , vitality =
      { initStat | name = "Vitality" }
  , endurance =
      { initStat | name = "Endurance" }
  , luck =
      { initStat | name = "Luck" }
  , heldAction = Nothing
  , hoveredUpgrade = Nothing
  , upgradeVelocity = 0
  }


initStat : Stat
initStat =
  { name = "INVALID STAT"
  , level = 1
  }


strength =
  create .strength <| \f m -> { m | strength = f m.strength }


speed =
  create .speed <| \f m -> { m | speed = f m.speed }


vitality =
  create .vitality <| \f m -> { m | vitality = f m.vitality }


endurance =
  create .endurance <| \f m -> { m | endurance = f m.endurance }


luck =
  create .luck <| \f m -> { m | luck = f m.luck }


invalidStat =
  create (always initStat) <| \_ m -> m


level =
  create .level <| \f s -> { s | level = f s.level }


update : Action -> Model -> ( Model, List Currency.Bundle )
update action model =
  let
    no m =
      ( m, [] )
  in
    case action of
      UpgradeAction action ->
        case action of
          UpgradeSlot.SetHeld action ->
            no { model | heldAction = Just action }

          UpgradeSlot.SetHover action ->
            no { model | hoveredUpgrade = Just action }

          UpgradeSlot.Release ->
            no
              { model
                | heldAction = Nothing
                , upgradeVelocity = 0
              }

          UpgradeSlot.MoveOut ->
            { model | hoveredUpgrade = Nothing }
              |> update (UpgradeAction UpgradeSlot.Release)

      Tick dT ->
        case model.heldAction of
          Just act ->
            let
              velModel =
                { model | upgradeVelocity = model.upgradeVelocity + dT }
            in
              upgradeBy (dT * velModel.upgradeVelocity) act velModel

          Nothing ->
            no model


upgradeBy : Float -> TimedAction -> Model -> ( Model, List Currency.Bundle )
upgradeBy amount action model =
  let
    ( limitedAmount, focus ) =
      case action of
        Upgrade focus ->
          ( amount, focus )

        LimitedUpgrade nextLevel focus ->
          let
            stat =
              get focus model
          in
            ( min amount <| toFloat nextLevel - stat.level
            , focus
            )

    stat =
      get focus model

    spent =
      cost limitedAmount focus model

    updatedStat =
      levelUp limitedAmount stat

    updatedModel =
      set focus updatedStat model
  in
    ( updatedModel, [ spent ] )


view : Signal.Address Action -> Equipment.Model -> Model -> Html
view address equip model =
  div
    []
    [ h3 [] [ text "Stats" ]
    , viewBaseStats address model
    , viewDerivedStats equip model
    ]


viewBaseStats : Signal.Address Action -> Model -> Html
viewBaseStats address model =
  let
    forwarded =
      Signal.forwardTo address UpgradeAction

    upgradeContext =
      { title = .name
      , level = .level
      , format = Format.currency
      , elem = li
      }

    viewStat focus =
      UpgradeSlot.viewStat upgradeContext cost Upgrade forwarded focus model

    items =
      List.map viewStat allStatFocuses
  in
    ul [] items


viewDerivedStats : Equipment.Model -> Model -> Html
viewDerivedStats equip model =
  let
    upgradedModel =
      case model.hoveredUpgrade of
        Just action ->
          fst <| upgradeBy 1 action model

        Nothing ->
          model

    viewStat ( title, format, stat ) =
      let
        curStat =
          stat model

        diff =
          stat upgradedModel - curStat

        diffSpan =
          if abs diff > 1.0e-2 then
            [ span
                [ style
                    [ color <| Color.rgb 32 200 64
                    , fontWeight Bold
                    ]
                ]
                [ text <| " (+" ++ format diff ++ ")" ]
            ]
          else
            []
      in
        li
          []
          ([ span [] [ text <| title ++ ": " ++ format curStat ] ]
            ++ diffSpan
          )

    i =
      Format.int << round

    f =
      Format.float

    items =
      List.map
        viewStat
        [ ( "Level", f, totalLevel )
        , ( "Power", i, toFloat << power << derived equip )
        , ( "Max Health", i, toFloat << maxHealth )
        , ( "Attack Damage", i, toFloat << attackDamage equip )
        , ( "Attack Speed", f, attackSpeed equip )
        , ( "Armor", i, toFloat << armor equip )
        , ( "Health Regen", f, healthRegen )
        , ( "DPS", f, \m -> attackSpeed equip m * toFloat (attackDamage equip m) )
        , ( "Gold Bonus %", f, goldBonus )
        ]
  in
    ul [] items


levelUp : Float -> Stat -> Stat
levelUp delta stat =
  { stat | level = stat.level + delta }


allStatFocuses : List (Focus Model Stat)
allStatFocuses =
  [ strength
  , speed
  , vitality
  , endurance
  , luck
  ]


allStats : Model -> List Stat
allStats model =
  allStatFocuses
    |> List.map (\f -> get f model)


allStatsCount : Int
allStatsCount =
  List.length allStatFocuses


totalLevel : Model -> Float
totalLevel model =
  allStats model
    |> List.map .level
    |> List.sum
    |> \n -> n / (toFloat allStatsCount)


cost : Float -> Focus Model Stat -> Model -> Currency.Bundle
cost delta focus model =
  ( Currency.Experience
  , Cost.cost totalCostValue delta (focus => level) model
  )


totalCostValue : Model -> Int
totalCostValue model =
  let
    cost stat =
      Cost.base ( 0.2, 1, 0 ) 1 stat.level

    level =
      totalLevel model

    baseCost =
      mapSum cost <| allStats model

    totalCost =
      sqrt level * baseCost
  in
    floor totalCost


attackDamage : Equipment.Model -> Model -> Int
attackDamage equip model =
  let
    wep =
      toFloat <| Equipment.attackDamage equip

    str =
      model.strength.level - 1

    strMod =
      1 + 0.1 * str

    lv =
      totalLevel model - 1

    lvMod =
      1 + 2.5e-2 * lv
  in
    round <| wep * strMod * lvMod


attackSpeed : Equipment.Model -> Model -> Float
attackSpeed equip model =
  let
    baseSpeed =
      Equipment.attackSpeed equip

    spd =
      model.speed.level - 1

    spdMod =
      1 + 8.0e-2 * spd
  in
    baseSpeed * spdMod


maxHealth : Model -> Int
maxHealth model =
  let
    vit =
      model.vitality.level - 1

    base =
      100 + 10 * vit

    lv =
      totalLevel model - 1

    lvMod =
      1 + 5.0e-2 * lv
  in
    round <| base * lvMod


healthRegen : Model -> Float
healthRegen model =
  let
    end =
      model.endurance.level - 1

    hp =
      toFloat <| maxHealth model
  in
    2 + 5.0e-3 * hp + 0.5 * end


armor : Equipment.Model -> Model -> Int
armor equip model =
  let
    arm =
      toFloat <| Equipment.armor equip

    end =
      model.endurance.level - 1

    endMod =
      1 + 0.1 * end
  in
    round <| arm * endMod


goldBonus : Model -> Float
goldBonus model =
  let
    lck =
      model.luck.level - 1
  in
    15 * lck


goldBonusMultiplier : Model -> Float
goldBonusMultiplier model =
  1 + goldBonus model / 100


derived : Equipment.Model -> Model -> Derived
derived equip model =
  { name = "Player"
  , image = "img/Player.png"
  , maxHealth = maxHealth model
  , healthRegen = healthRegen model
  , attackDamage = attackDamage equip model
  , attackSpeed = attackSpeed equip model
  , armor = armor equip model
  }


power : Derived -> Int
power stats =
  let
    hp =
      toFloat <| stats.maxHealth

    atk =
      toFloat <| stats.attackDamage

    spd =
      stats.attackSpeed
  in
    round <| (hp * atk * spd) ^ 0.4


serializer : Serialize.Serializer Model
serializer =
  let
    statData focus =
      let
        stat =
          get focus init
      in
        ( stat.name, focus => level, Serialize.float )

    data =
      List.map statData allStatFocuses
  in
    Serialize.foldList data init
