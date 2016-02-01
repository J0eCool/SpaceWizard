module Mana (..) where

import Color
import Focus
import Html exposing (Html, div, span, h3, text, ul, li, button)
import Html.Events exposing (onClick)
import Cost
import Format
import Serialize
import Widgets.ProgressBar as ProgressBar
import Widgets.UpgradeSlot as UpgradeSlot


type alias Model =
  { current : Float
  , capacityLevel : Float
  , regenLevel : Float
  , heldAction : Maybe TimedAction
  , upgradeVelocity : Float
  }


type Action
  = Tick Float
  | UpgradeAction (UpgradeSlot.Action TimedAction)


type TimedAction
  = Upgrade (Focus.Focus Model Float)


init : Model
init =
  { current = 0
  , capacityLevel = 1
  , regenLevel = 1
  , heldAction = Nothing
  , upgradeVelocity = 1
  }


current =
  Focus.create .current (\f m -> { m | current = f m.current })


capacityLevel =
  Focus.create .capacityLevel (\f m -> { m | capacityLevel = f m.capacityLevel })


regenLevel =
  Focus.create .regenLevel (\f m -> { m | regenLevel = f m.regenLevel })


update : Action -> Model -> Model
update action model =
  case action of
    Tick dT ->
      let
        r =
          regen model

        cur =
          model.current
            + r
            * dT
            |> min (toFloat (capacity model))
      in
        { model | current = cur }
          |> case model.heldAction of
              Nothing ->
                identity

              Just action ->
                updateUpgrade dT action

    UpgradeAction action ->
      case action of
        UpgradeSlot.SetHeld act ->
          { model | heldAction = Just act }

        UpgradeSlot.SetHover act ->
          model

        UpgradeSlot.Release ->
          { model | heldAction = Nothing, upgradeVelocity = 0 }

        UpgradeSlot.MoveOut ->
          update (UpgradeAction UpgradeSlot.Release) model


updateUpgrade dT action model =
  case action of
    Upgrade focus ->
      let
        vel =
          model.upgradeVelocity + dT

        amt =
          vel * dT

        level =
          Focus.get focus model

        upgraded =
          level + amt

        upgradeCost =
          cost amt focus model

        tickedModel =
          { model | upgradeVelocity = vel }

        upgradedModel =
          Focus.set focus upgraded tickedModel
      in
        trySpend upgradeCost upgradedModel model


view : Signal.Address Action -> Model -> Html
view address model =
  let
    manaBar =
      { width = 600
      , height = 14
      , curAmount = model.current
      , maxAmount = toFloat <| capacity model
      , color = Color.rgb 16 128 255
      , background = Color.rgb 16 16 92
      }
  in
    div
      []
      [ h3 [] [ text "Mana" ]
      , text <| Format.int (floor model.current) ++ "/" ++ Format.int (capacity model)
      , ProgressBar.view manaBar
      , viewBaseStats address model
      , viewDerivedStats model
      ]


viewBaseStats address model =
  let
    forwarded =
      Signal.forwardTo address UpgradeAction

    viewStat name focus =
      let
        upgradeContext =
          { title = always name
          , level = identity
          , format = \c -> Format.int c ++ " Mana"
          , elem = li
          }
      in
        UpgradeSlot.viewStat upgradeContext cost Upgrade forwarded focus model
  in
    ul
      []
      [ viewStat "Capacity" capacityLevel
      , viewStat "Regen" regenLevel
      ]


viewDerivedStats model =
  ul
    []
    [ li [] [ text <| "Mana regen: +" ++ Format.float (regen model) ++ "/s" ]
    ]


capacity : Model -> Int
capacity model =
  let
    lv =
      model.capacityLevel - 1
  in
    floor <| 1000 + 50 * lv


regen : Model -> Float
regen model =
  let
    lv =
      model.regenLevel - 1
  in
    0.8 + 0.1 * lv


cost delta focus model =
  Cost.cost totalCost delta (focus) model


totalCost : Model -> Int
totalCost model =
  let
    totalCost =
      List.map
        round
        [ let
            lv =
              model.capacityLevel - 1
          in
            80 * lv + 20 * lv * lv
        , let
            lv =
              model.regenLevel - 1
          in
            92 * lv + 8 * lv * lv * lv
        ]
        |> List.sum
  in
    totalCost


canSpend : Int -> Model -> Bool
canSpend cost model =
  toFloat cost <= model.current


trySpend : Int -> Model -> Model -> Model
trySpend cost upgraded model =
  if canSpend cost upgraded then
    { upgraded | current = upgraded.current - toFloat cost }
  else
    model


serializer : Serialize.Serializer Model
serializer =
  Serialize.foldList
    [ ( "Regen", regenLevel, Serialize.float )
    , ( "Capacity", capacityLevel, Serialize.float )
    , ( "Current", current, Serialize.float )
    ]
    init
