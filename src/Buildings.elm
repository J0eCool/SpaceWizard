module Buildings where

import Html exposing (Html, div)

import Currency

type alias Model =
  { buildings : List (Int, Building)
  }

type alias Building =
  { name : String
  , production : Currency.FloatBundle
  }

type Action
  = Tick Float

init : Model
init =
  { buildings =
    [ ( 1
      , { name = "Miner"
        , production = (Currency.Iron, 0.5)
        }
      )]
  }

update : Action -> Model -> (Model, List Currency.FloatBundle)
update action model =
  case action of
    Tick dT ->
      (model, totalProduction dT model)

view : Signal.Address Action -> Model -> Html
view address model =
  div [] []

totalProduction : Float -> Model -> List Currency.FloatBundle
totalProduction dT model =
  let
    production (count, building) =
      let (t, base) = building.production
      in (t, dT * toFloat count * base)
  in List.map production model.buildings
