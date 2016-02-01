module Buildings (..) where

import Focus exposing (Focus, (=>))
import Html exposing (Html, div, span, h3, text, ul, li, button)
import Html.Events exposing (onClick)
import Cost
import Currency
import Format
import ListUtil exposing (mapSum)
import Serialize
import Widgets.UpgradeSlot as UpgradeSlot


type alias Model =
  { buildings : List Building
  , heldAction : Maybe TimedAction
  , upgradeVelocity : Float
  }


type alias Building =
  { name : String
  , count : Int
  , level : Float
  , manaLevel : Float
  , production : Currency.FloatBundle
  , baseCost : Currency.Bundle
  , upgradeCost : Currency.Bundle
  , manaCost : Int
  }


type Action
  = Tick Float
  | Buy Building
  | UpgradeAction (UpgradeSlot.Action TimedAction)


type TimedAction
  = Upgrade (Focus Model Building)
  | ManaUpgrade (Focus Model Building)


type alias Effect =
  { reward : List Currency.FloatBundle
  , cost : List Currency.Bundle
  , manaCost : Int
  }


init : Model
init =
  { buildings =
      [ { name = "Gold Miner"
        , count = 0
        , level = 1
        , manaLevel = 1
        , production = ( Currency.Gold, 1 )
        , baseCost = ( Currency.Gold, 1000 )
        , upgradeCost = ( Currency.Gold, 10000 )
        , manaCost = 100
        }
      , { name = "Iron Miner"
        , count = 0
        , level = 1
        , manaLevel = 1
        , production = ( Currency.Iron, 0.5 )
        , baseCost = ( Currency.Gold, 100 )
        , upgradeCost = ( Currency.Iron, 1000 )
        , manaCost = 100
        }
      , { name = "Aluminum Miner"
        , count = 0
        , level = 1
        , manaLevel = 1
        , production = ( Currency.Aluminum, 0.4 )
        , baseCost = ( Currency.Iron, 250 )
        , upgradeCost = ( Currency.Aluminum, 1000 )
        , manaCost = 100
        }
      , { name = "Steel Smelter"
        , count = 0
        , level = 1
        , manaLevel = 1
        , production = ( Currency.Steel, 0.3 )
        , baseCost = ( Currency.Aluminum, 750 )
        , upgradeCost = ( Currency.Steel, 1000 )
        , manaCost = 100
        }
      ]
  , heldAction = Nothing
  , upgradeVelocity = 0
  }


update : Action -> Model -> ( Model, Effect )
update action model =
  let
    rewardEffect r m =
      ( m, { reward = r, cost = [], manaCost = 0 } )

    costEffect c m =
      ( m, { reward = [], cost = c, manaCost = 0 } )

    no m =
      ( m, { reward = [], cost = [], manaCost = 0 } )
  in
    case action of
      Tick dT ->
        updateTick dT model

      Buy building ->
        let
          inc b =
            if building == b then
              { b | count = b.count + 1 }
            else
              b

          updatedBuildings =
            List.map inc model.buildings

          updatedModel =
            { model | buildings = updatedBuildings }

          cost =
            [ purchaseCost building ]
        in
          costEffect cost updatedModel

      UpgradeAction action ->
        case action of
          UpgradeSlot.SetHeld act ->
            no { model | heldAction = Just act }

          UpgradeSlot.SetHover act ->
            no model

          UpgradeSlot.Release ->
            no { model | heldAction = Nothing, upgradeVelocity = 0 }

          UpgradeSlot.MoveOut ->
            update (UpgradeAction UpgradeSlot.Release) model


updateTick : Float -> Model -> ( Model, Effect )
updateTick dT model =
  let
    ( upgradedModel, cost, manaCost ) =
      case model.heldAction of
        Nothing ->
          ( model, [], 0 )

        Just action ->
          updateUpgrade dT action model

    reward =
      totalProduction dT model
  in
    ( upgradedModel
    , { reward = reward
      , cost = cost
      , manaCost = manaCost
      }
    )


updateUpgrade : Float -> TimedAction -> Model -> ( Model, List Currency.Bundle, Int )
updateUpgrade dT action model =
  let
    vel =
      model.upgradeVelocity + dT

    amt =
      vel * dT

    tickedModel =
      { model | upgradeVelocity = vel }
  in
    case action of
      Upgrade focus ->
        let
          building =
            Focus.get focus model

          upgraded =
            { building | level = building.level + amt }

          cost =
            upgradeCost amt focus model
        in
          ( Focus.set focus upgraded tickedModel, [ cost ], 0 )

      ManaUpgrade focus ->
        let
          building =
            Focus.get focus model

          upgraded =
            { building | manaLevel = building.manaLevel + amt }

          cost =
            manaUpgradeCost amt focus model
        in
          ( Focus.set focus upgraded tickedModel, [], cost )


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ h3 [] [ text "Buildings" ]
    , ul [] <| List.map (viewBuilding address model) model.buildings
    ]


viewBuilding : Signal.Address Action -> Model -> Building -> Html
viewBuilding address model building =
  let
    focus =
      focusFor building

    forwarded =
      Signal.forwardTo address UpgradeAction

    upgradeContext =
      { title = always "Level"
      , level = .level
      , format = Format.currency
      , elem = div
      }

    manaUpgradeContext =
      { title = always "Mana Level"
      , level = .manaLevel
      , format = \c -> Format.int c ++ " Mana"
      , elem = div
      }
  in
    li
      []
      [ div [] [ text building.name ]
      , div [] [ text <| "Owned: " ++ Format.int building.count ]
      , div
          []
          [ text
              <| "+"
              ++ Format.floatCurrency (individualProduction building)
              ++ "/s each"
          ]
      , div
          []
          [ text
              <| "(Total: "
              ++ Format.floatCurrency (production building)
              ++ "/s)"
          ]
      , div
          []
          [ button
              [ onClick address <| Buy building ]
              [ text <| "Buy (" ++ Format.currency (purchaseCost building) ++ ")" ]
          ]
      , UpgradeSlot.viewStat upgradeContext upgradeCost Upgrade forwarded focus model
      , UpgradeSlot.viewStat manaUpgradeContext manaUpgradeCost ManaUpgrade forwarded focus model
      ]


individualProduction : Building -> Currency.FloatBundle
individualProduction building =
  let
    ( t, base ) =
      building.production

    lv =
      2 ^ (building.level - 1)

    mana =
      building.manaLevel

    amount =
      base * lv * mana
  in
    ( t, amount )


production : Building -> Currency.FloatBundle
production building =
  let
    ( t, individual ) =
      individualProduction building

    count =
      toFloat building.count
  in
    ( t, count * individual )


totalProduction : Float -> Model -> List Currency.FloatBundle
totalProduction dT model =
  let
    notZero building =
      building.count > 0
  in
    model.buildings
      |> List.filter notZero
      |> List.map production
      |> List.map (\( t, x ) -> ( t, x * dT ))


purchaseCost : Building -> Currency.Bundle
purchaseCost building =
  let
    ( t, base ) =
      building.baseCost
  in
    ( t, floor <| toFloat base * 1.067 ^ (toFloat building.count ^ 0.9) )


upgradeCost : Float -> Focus Model Building -> Model -> Currency.Bundle
upgradeCost delta focus model =
  ( Focus.get focus model
      |> .upgradeCost
      |> fst
  , Cost.cost totalUpgradeCost delta (focus => level) model
  )


totalUpgradeCost : Model -> Int
totalUpgradeCost model =
  let
    buildingCost building =
      let
        lv =
          building.level - 1

        mult =
          3.25
      in
        floor <| toFloat (snd building.upgradeCost) * (mult ^ lv) * lv / mult
  in
    mapSum buildingCost model.buildings


manaUpgradeCost : Float -> Focus Model Building -> Model -> Int
manaUpgradeCost delta focus model =
  Cost.cost totalManaUpgradeCost delta (focus => manaLevel) model


totalManaUpgradeCost : Model -> Int
totalManaUpgradeCost model =
  let
    buildingCost building =
      let
        lv =
          building.manaLevel - 1
      in
        floor <| toFloat building.manaCost * (0.5 * lv + 0.4 * lv * lv + 0.1 * lv * lv * lv)
  in
    mapSum buildingCost model.buildings


buildingSerializer : Building -> Serialize.Serializer Building
buildingSerializer building =
  Serialize.object3
    building
    ( "count", Focus.create .count (\f b -> { b | count = f b.count }), Serialize.int )
    ( "level", Focus.create .level (\f b -> { b | level = f b.level }), Serialize.float )
    ( "manaLevel", Focus.create .manaLevel (\f b -> { b | manaLevel = f b.manaLevel }), Serialize.float )


focusFor : Building -> Focus.Focus Model Building
focusFor building =
  Serialize.namedListFocus .buildings (\bs m -> { m | buildings = bs }) init building


level =
  Focus.create .level (\f b -> { b | level = f b.level })


manaLevel =
  Focus.create .manaLevel (\f b -> { b | manaLevel = f b.manaLevel })


serializer : Serialize.Serializer Model
serializer =
  let
    buildingData building =
      ( building.name, focusFor building, buildingSerializer building )

    data =
      List.map buildingData init.buildings
  in
    Serialize.foldList data init
