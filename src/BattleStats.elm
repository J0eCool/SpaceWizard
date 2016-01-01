module BattleStats where

import Color
import Html exposing (Html, div, h3, text, span, button, ul, li)
import Html.Events exposing (onMouseDown, onMouseUp, onMouseEnter, onMouseLeave)

import Currency
import Equipment
import Format
import Style exposing (..)
import Widgets.ProgressBar as ProgressBar

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
  { maxHealth : Int
  , healthRegen : Float
  , attackDamage : Int
  , attackSpeed : Float
  , armor : Int
  }

type WrapModel =
  WrapModel Model

type alias Stat =
  { level : Float
  , setter : WrapStat -> WrapModel -> WrapModel
  }

type WrapStat =
  WrapStat Stat

type Action
  = SetHeld TimedAction
  | SetHover TimedAction
  | Release
  | MoveOut
  | Tick Float

type TimedAction
  = Upgrade (Model -> Stat)
  | LimitedUpgrade Int (Model -> Stat)

init : Model
init =
  initWith 1 1 1 1 1

initWith : Float -> Float -> Float -> Float -> Float -> Model
initWith str spd vit end lck =
  { strength =
    { level = str
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | strength = stat })
    }
  , speed =
    { level = spd
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | speed = stat })
    }
  , vitality =
    { level = vit
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | vitality = stat })
    }
  , endurance =
    { level = end
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | endurance = stat })
    }
  , luck =
    { level = lck
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | luck = stat })
    }
  , heldAction = Nothing
  , hoveredUpgrade = Nothing
  , upgradeVelocity = 0
  }

update : Action -> Model -> (Model, List Currency.Bundle)
update action model =
  let no m = (m, [])
  in case action of
    SetHeld action ->
      no { model | heldAction = Just action }
    SetHover action ->
      no { model | hoveredUpgrade = Just action }
    Release ->
      no { model
        | heldAction = Nothing
        , upgradeVelocity = 0
        }
    MoveOut ->
      { model | hoveredUpgrade = Nothing }
        |> update Release
    Tick dT ->
      case model.heldAction of
        Just act ->
          let velModel =
            { model | upgradeVelocity = model.upgradeVelocity + dT }
          in upgradeBy (dT * velModel.upgradeVelocity) act velModel
        Nothing ->
          no model

upgradeBy : Float -> TimedAction -> Model -> (Model, List Currency.Bundle)
upgradeBy amount action model =
  let
    (limitedAmount, stat) =
      case action of
        Upgrade field ->
          (amount, field model)
        LimitedUpgrade nextLevel field ->
          let stat = field model
          in
            ( min amount <| toFloat nextLevel - stat.level
            , stat
            )
    spent =
      cost limitedAmount stat model
    updatedStat =
      levelUp limitedAmount stat
    (WrapModel updatedModel) =
      stat.setter (WrapStat updatedStat) (WrapModel model)
  in (updatedModel, [spent])

view : Signal.Address Action -> Equipment.Model -> Model -> Html
view address equip model =
  div []
    [ h3 [] [text "Stats"]
    , viewBaseStats address model
    , viewDerivedStats equip model
    ]

viewBaseStats : Signal.Address Action -> Model -> Html
viewBaseStats address model =
  let
    viewStat (title, field) =
      let
        stat =
          field model
        curCost =
          cost 1 stat model
        upgradeButton delta label action =
          let curCost =
            cost delta stat model
          in
            button
            [ onMouseDown address <| SetHeld action
            , onMouseEnter address <| SetHover action
            , onMouseUp address <| Release
            , onMouseLeave address <| MoveOut
            ]
            [ text
              <| label
              ++ " ("
              ++ Format.currency curCost
              ++ ")"
            ]
        next =
          ceiling <| stat.level + 0.000001
        toNext =
          toFloat next - stat.level
      in
        li []
        [ span 
          [ style
            [ display InlineBlock
            , width <| Px 120
            ]
          ]
          [ text
            <| title ++ ": "
            ++ Format.float stat.level
          ]
        , ProgressBar.viewInline
          { width = 200
          , height = 10
          , color = Color.rgb 64 255 200
          , background = Color.rgb 32 128 92
          , curAmount = stat.level - toFloat (floor stat.level)
          , maxAmount = 1
          }
        , upgradeButton 1 "Upgrade" (Upgrade field)
        , upgradeButton toNext ("To " ++ Format.int next) (LimitedUpgrade next field)
        ]
    items =
      List.map viewStat
        [ ("Strength", .strength)
        , ("Speed", .speed)
        , ("Vitality", .vitality)
        , ("Endurance", .endurance)
        , ("Luck", .luck)
        ]
  in ul [] items

viewDerivedStats : Equipment.Model -> Model -> Html
viewDerivedStats equip model =
  let
    upgradedModel =
      case model.hoveredUpgrade of
        Just action ->
          fst <| upgradeBy 1 action model
        Nothing ->
          model
    viewStat (title, format, stat) =
      let
        curStat =
          stat model
        diff =
          stat upgradedModel - curStat
        diffSpan =
          if abs diff > 0.01 then
            [ span
              [style
                [ color <| Color.rgb 32 200 64 
                , fontWeight Bold
                ]
              ]
              [text <| " (+" ++ format diff ++ ")"] ]
          else
            []
      in li []
        ([ span [] [text <| title ++ ": " ++ format curStat] ]
        ++ diffSpan
        )
    i = Format.int << round
    f = Format.float
    items =
      List.map viewStat
        [ ("Level", f, level)
        , ("Max Health", i, toFloat << maxHealth)
        , ("Attack Damage", i, toFloat << attackDamage equip)
        , ("Attack Speed", f, attackSpeed equip)
        , ("Armor", i, toFloat << armor equip)
        , ("Health Regen", f, healthRegen)
        , ("DPS", f, \m -> attackSpeed equip m * toFloat (attackDamage equip m))
        , ("Gold Bonus %", f, goldBonus)
        ]
  in ul [] items

levelUp : Float -> Stat -> Stat
levelUp delta stat =
  { stat | level = stat.level + delta }

cost : Float -> Stat -> Model -> Currency.Bundle
cost delta stat model =
  let
    wrapModel = WrapModel model
    cur = totalCostValue wrapModel
    nextStat = WrapStat { stat | level = stat.level + delta }
    next = totalCostValue (stat.setter nextStat wrapModel)
  in
    ( Currency.Experience
    , next - cur
    )

allStatFields : List (Model -> Stat)
allStatFields =
  [ .strength
  , .speed
  , .vitality
  , .endurance
  , .luck
  ]

allStats : Model -> List Stat
allStats model =
  allStatFields
    |> List.map (\f -> f model)

allStatsCount : Int
allStatsCount =
  List.length allStatFields

level : Model -> Float
level model =
  allStats model
    |> List.map .level
    |> List.sum
    |> \n -> n / (toFloat allStatsCount)

totalCostValue : WrapModel -> Int
totalCostValue (WrapModel model) =
  let
    (a, b, c) =
      (0, 1, 0)
    cost stat =
      let l = stat.level
      in l * (c + l * (b + l * a)) -- ax^3 + bx^2 + cx
    totalLevel =
      level model
    baseCost =
      allStats model
        |> List.map cost
        |> List.sum
    totalCost =
      totalLevel * baseCost
  in floor totalCost

attackDamage : Equipment.Model -> Model -> Int
attackDamage equip model =
  let
    wep = toFloat <| Equipment.attackDamage equip
    str = model.strength.level - 1
    strMod = 1 + 0.1 * str
    lv = level model - 1
    lvMod = 1 + 0.025 * lv
  in round <| wep * strMod * lvMod

attackSpeed : Equipment.Model -> Model -> Float
attackSpeed equip model =
  let
    baseSpeed = Equipment.attackSpeed equip
    spd = model.speed.level - 1
    spdMod = 1 + 0.08 * spd
  in baseSpeed * spdMod

maxHealth : Model -> Int
maxHealth model =
  let
    vit = model.vitality.level - 1
    base = 100 + 10 * vit
    lv = level model - 1
    lvMod = 1 + 0.05 * lv
  in round <| base * lvMod

healthRegen : Model -> Float
healthRegen model =
  let
    end = model.endurance.level - 1
    hp = toFloat <| maxHealth model
  in 2 + 0.005 * hp + 0.5 * end

armor : Equipment.Model -> Model -> Int
armor equip model =
  let
    arm = toFloat <| Equipment.armor equip
    end = model.endurance.level - 1
    endMod = 1 + 0.1 * end
  in round <| arm * endMod

goldBonus : Model -> Float
goldBonus model =
  let lck = model.luck.level - 1
  in 15 * lck

goldBonusMultiplier : Model -> Float
goldBonusMultiplier model =
  1 + goldBonus model / 100

derived : Equipment.Model -> Model -> Derived
derived equip model =
  { maxHealth = maxHealth model
  , healthRegen = healthRegen model
  , attackDamage = attackDamage equip model
  , attackSpeed = attackSpeed equip model
  , armor = armor equip model
  }
