module BattleStats where

import Color
import Focus exposing (..)
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

type alias Stat =
  { level : Float
  , name : String
  }

type Action
  = SetHeld TimedAction
  | SetHover TimedAction
  | Release
  | MoveOut
  | Tick Float

type TimedAction
  = Upgrade (Focus Model Stat)
  | LimitedUpgrade Int (Focus Model Stat)

init : Model
init =
  { strength =
    { name = "Strength"
    , level = 1
    }
  , speed =
    { name = "Speed"
    , level = 1
    }
  , vitality =
    { name = "Vitality"
    , level = 1
    }
  , endurance =
    { name = "Endurance"
    , level = 1
    }
  , luck =
    { name = "Luck"
    , level = 1
    }
  , heldAction = Nothing
  , hoveredUpgrade = Nothing
  , upgradeVelocity = 0
  }

strength = create .strength <| \f m -> { m | strength = f m.strength }
speed = create .speed <| \f m -> { m | speed = f m.speed }
vitality = create .vitality <| \f m -> { m | vitality = f m.vitality }
endurance = create .endurance <| \f m -> { m | endurance = f m.endurance }
luck = create .luck <| \f m -> { m | luck = f m.luck }

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
    (limitedAmount, focus) =
      case action of
        Upgrade focus ->
          (amount, focus)
        LimitedUpgrade nextLevel focus ->
          let stat = get focus model
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
    viewStat focus =
      let
        stat =
          get focus model
        title =
          stat.name
        curCost =
          cost 1 focus model
        upgradeButton delta label action =
          let curCost =
            cost delta focus model
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
        , upgradeButton 1 "Upgrade" (Upgrade focus)
        , upgradeButton toNext ("To " ++ Format.int next) (LimitedUpgrade next focus)
        ]
    items =
      List.map viewStat allStatFocuses
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

cost : Float -> Focus Model Stat -> Model -> Currency.Bundle
cost delta focus model =
  let
    stat = get focus model
    cur = totalCostValue model
    nextStat = { stat | level = stat.level + delta }
    next = totalCostValue (set focus nextStat model)
  in
    ( Currency.Experience
    , next - cur
    )

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

level : Model -> Float
level model =
  allStats model
    |> List.map .level
    |> List.sum
    |> \n -> n / (toFloat allStatsCount)

totalCostValue : Model -> Int
totalCostValue model =
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
