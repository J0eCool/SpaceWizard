module BattleStats where

import Color
import Html exposing (Html, div, h3, text, span, button, ul, li)
import Html.Events exposing (onMouseDown, onMouseUp, onMouseEnter, onMouseLeave)

import Currency
import Format
import Style exposing (..)
import Widgets.ProgressBar as ProgressBar

type alias Model =
  { strength : Stat
  , speed : Stat
  , vitality : Stat
  , luck : Stat
  , weapon : Stat
  , armor : Stat
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

type Cost
  = TotalCost Float Float Float

type alias CostBundle =
  (Currency.Type, Cost)

type alias Stat =
  { level : Float
  , cost : CostBundle
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
initWith str spd vit lck wep =
  { strength =
    { level = str
    , cost = baseStatCost
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | strength = stat })
    }
  , speed =
    { level = spd
    , cost = baseStatCost
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | speed = stat })
    }
  , vitality =
    { level = vit
    , cost = baseStatCost
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | vitality = stat })
    }
  , luck =
    { level = lck
    , cost = baseStatCost
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | luck = stat })
    }
  , weapon =
    { level = wep
    , cost =
      ( Currency.Gold
      , TotalCost 2 2 10
      )
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | weapon = stat })
    }
  , armor =
    { level = wep
    , cost =
      ( Currency.Gold
      , TotalCost 2 2 10
      )
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | armor = stat })
    }
  , heldAction = Nothing
  , hoveredUpgrade = Nothing
  , upgradeVelocity = 0
  }

baseStatCost : CostBundle
baseStatCost =
  ( Currency.Experience
  , TotalCost 0 1 0
  )

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

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h3 [] [text "Stats"]
    , viewBaseStats address model
    , viewDerivedStats model
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
        , ("Luck", .luck)
        , ("Weapon", .weapon)
        , ("Armor", .armor)
        ]
  in ul [] items

viewDerivedStats : Model -> Html
viewDerivedStats model =
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
        , ("Attack Damage", i, toFloat << attackDamage)
        , ("Attack Speed", f, attackSpeed)
        , ("Armor", i, toFloat << armor)
        , ("DPS", f, \m -> attackSpeed m * toFloat (attackDamage m))
        , ("Weapon base damage", i, toFloat << weaponDamage)
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
  , .luck
  ]

allStats : Model -> List Stat
allStats model =
  allStatFields
    |> List.map (\f -> f model)


level : Model -> Float
level model =
  allStats model
    |> List.map .level
    |> List.sum
    |> flip (/) 4

totalCostValue : WrapModel -> Int
totalCostValue (WrapModel model) =
  let
    (TotalCost a b c) =
      snd baseStatCost
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

weaponDamage : Model -> Int
weaponDamage model =
  let wep = model.weapon.level - 1
  in round <| 20 + 5 * wep

attackDamage : Model -> Int
attackDamage model =
  let
    wep = toFloat <| weaponDamage model
    str = model.strength.level - 1
    strMod = 1 + 0.1 * str
  in round <| wep * strMod

attackSpeed : Model -> Float
attackSpeed model =
  let spd = model.speed.level - 1
  in 1.2 + 0.1 * spd

maxHealth : Model -> Int
maxHealth model =
  let vit = model.vitality.level - 1
  in round <| 100 + 10 * vit

armor : Model -> Int
armor model =
  let arm = model.armor.level - 1
  in round <| 5 + arm

goldBonus : Model -> Float
goldBonus model =
  let lck = model.luck.level - 1
  in 15 * lck

goldBonusMultiplier : Model -> Float
goldBonusMultiplier model =
  1 + goldBonus model / 100

derived : Model -> Derived
derived model =
  { maxHealth = maxHealth model
  , healthRegen = 0
  , attackDamage = attackDamage model
  , attackSpeed = attackSpeed model
  , armor = armor model
  }
