module Buildings where

import Html exposing (Html, div, span, h3, text, ul, li, button)
import Html.Events exposing (onClick)

import Currency
import Format

type alias Model =
  { buildings : List Building
  }

type alias Building =
  { name : String
  , count : Int
  , production : Currency.FloatBundle
  , baseCost : Currency.Bundle
  }

type Action
  = Tick Float
  | Buy Building

type alias Effect =
  { reward : List Currency.FloatBundle
  , cost : List Currency.Bundle
  }

init : Model
init =
  { buildings =
    [ { name = "Iron Miner"
      , count = 0
      , production = (Currency.Iron, 0.5)
      , baseCost = (Currency.Gold, 100)
      }
    , { name = "Aluminum Miner"
      , count = 0
      , production = (Currency.Aluminum, 0.4)
      , baseCost = (Currency.Iron, 10)
      }
    ]
  }

update : Action -> Model -> (Model, Effect)
update action model =
  let
    rewardEffect r m =
      (m, { reward = r, cost = [] })
    costEffect c m =
      (m, { reward = [], cost = c })
  in case action of
    Tick dT ->
      rewardEffect (totalProduction dT model) model
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
          [purchaseCost building]
      in costEffect cost updatedModel

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h3 [] [text "Buildings"]
    , ul [] <| List.map (viewBuilding address) model.buildings
    ]

viewBuilding : Signal.Address Action -> Building -> Html
viewBuilding address building =
  li []
    [ div [] [text building.name]
    , div [] [text <| "Owned: " ++ Format.int building.count]
    , div [] [text <| "+" ++ Format.floatCurrency building.production ++ "/s"]
    , div [] [button [onClick address <| Buy building]
        [text <| "Buy (" ++ Format.currency (purchaseCost building) ++ ")"]]
    ]

totalProduction : Float -> Model -> List Currency.FloatBundle
totalProduction dT model =
  let
    production building =
      let (t, base) = building.production
      in (t, dT * toFloat building.count * base)
    notZero (_, amount) =
      amount > 0
  in
    model.buildings
      |> List.map production
      |> List.filter notZero

purchaseCost : Building -> Currency.Bundle
purchaseCost building =
  let (t, base) = building.baseCost
  in (t, floor <| toFloat base * 1.067 ^ (toFloat building.count ^ 0.9))
