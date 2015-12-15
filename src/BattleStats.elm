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
  , heldAction : Maybe TimedAction
  , hoveredUpgrade : Maybe TimedAction
  }

type WrapModel =
  WrapModel Model

type Growth
  = LinearGrowth Float Float
  | PowerGrowth Float Float Float

type Cost
  = TotalCost Float Float Float

type alias CostBundle =
  (Currency.Type, Cost)

type alias Stat =
  { level : Float
  , growth : Growth
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

init : Model
init =
  initWith 1 1 1 1 1

initWith : Float -> Float -> Float -> Float -> Float -> Model
initWith str spd vit lck wep =
  { strength =
    { level = str
    , growth = LinearGrowth 1 0.1
    , cost = baseStatCost
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | strength = stat })
    }
  , speed =
    { level = spd
    , growth = LinearGrowth 1.2 0.1
    , cost = baseStatCost
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | speed = stat })
    }
  , vitality =
    { level = vit
    , growth = LinearGrowth 100 10
    , cost = baseStatCost
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | vitality = stat })
    }
  , luck =
    { level = lck
    , growth = LinearGrowth 0 15
    , cost = baseStatCost
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | luck = stat })
    }
  , weapon =
    { level = wep
    , growth = LinearGrowth 20 5
    , cost =
      ( Currency.Gold
      , TotalCost 2 2 1
      )
    , setter = \(WrapStat stat) (WrapModel model) ->
        (WrapModel { model | weapon = stat })
    }
  , heldAction = Nothing
  , hoveredUpgrade = Nothing
  }

baseStatCost : CostBundle
baseStatCost =
  ( Currency.Experience
  , TotalCost 0 1 (-2)
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
      no { model | heldAction = Nothing }
    MoveOut ->
      { model | hoveredUpgrade = Nothing }
        |> update Release
    Tick dT ->
      case model.heldAction of
        Just act ->
          upgradeBy dT act model
        Nothing ->
          no model

upgradeBy : Float -> TimedAction -> Model -> (Model, List Currency.Bundle)
upgradeBy amount action model =
  let
    stat =
      case action of
        Upgrade field ->
          field model
    spent =
      cost amount stat model
    updatedStat =
      levelUp amount stat
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
        stat = field model
        curCost = cost 1 stat model
        action = Upgrade field
      in li []
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
        , button
          [ onMouseDown address <| SetHeld action
          , onMouseEnter address <| SetHover action
          , onMouseUp address <| Release
          , onMouseLeave address <| MoveOut
          ]
          [ text
            <| "+1 ("
            ++ Format.currency curCost
            ++ ")"
          ]
        ]
    items =
      List.map viewStat
        [ ("Strength", .strength)
        , ("Speed", .speed)
        , ("Vitality", .vitality)
        , ("Luck", .luck)
        , ("Weapon", .weapon)
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
        , ("DPS", f, \m -> attackSpeed m * toFloat (attackDamage m))
        , ("Weapon base damage", i, toFloat << weaponDamage)
        , ("Gold Bonus %", f, goldBonus)
        ]
  in ul [] items

levelUp : Float -> Stat -> Stat
levelUp delta stat =
  { stat | level = stat.level + delta }

value : Stat -> Float
value stat =
  growthValue stat.level stat.growth

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

growthValue : Float -> Growth -> Float
growthValue level growth =
  case growth of
    LinearGrowth base slope ->
      slope * (level - 1) + base
    PowerGrowth lin powFac pow ->
      lin * level + powFac * (level - 1) ^ pow

weaponDamage : Model -> Int
weaponDamage model =
  round <| value model.weapon

attackDamage : Model -> Int
attackDamage model =
  let
    wep = toFloat <| weaponDamage model
    str = value model.strength
  in round <| wep * str

attackSpeed : Model -> Float
attackSpeed model =
  value model.speed

maxHealth : Model -> Int
maxHealth model =
  round <| value model.vitality

goldBonus : Model -> Float
goldBonus model =
  value model.luck

goldBonusMultiplier : Model -> Float
goldBonusMultiplier model =
  1 + goldBonus model / 100
