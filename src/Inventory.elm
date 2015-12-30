module Inventory where

import Dict
import Html exposing (Html, h3, text, div, ul, li, span)

import Currency
import Format

type alias Model =
  Dict.Dict Int Int -- Dict Type Int

init : Model
init =
  [ (Currency.Gold, 0)
  , (Currency.Experience, 0)
  ]
    |> List.map Currency.bundleToEnum
    |> Dict.fromList

update : List Currency.Bundle -> Model -> Model
update rewards model =
  applyRewards rewards model

view : Model -> Html
view model =
  div []
    [ h3 [] [text "Currency"]
    ,   let lines = map viewCurrency model
      in ul [] lines
    ]

viewCurrency : Currency.Bundle -> Html
viewCurrency (t, amount) =
  li []
    [ span [] [text <| toString t ++ ":"]
    , span [] [text <| Format.int amount]
    ]

get : Currency.Type -> Model -> Int
get t model =
  Dict.get (Currency.toEnum t) model
    |> Maybe.withDefault 0

set : Currency.Bundle -> Model -> Model
set (t, amount) model =
  Dict.update (Currency.toEnum t) (\_ -> Just amount) model

gain : Currency.Bundle -> Model -> Model
gain (t, amount) model =
  let cur = get t model
  in set (t, cur + amount) model

spend : Currency.Bundle -> Model -> Result String Model
spend (t, cost) model =
  let
    curAmt = get t model
    canAfford = curAmt >= cost
  in  if canAfford then
      Ok <| set (t, curAmt - cost) model
    else
      Err <| "Not enough " ++ toString t

spendAll : List Currency.Bundle -> Model -> Result String Model
spendAll costs model =
  let f cost res =
    case res of
      Ok inv ->
        spend cost inv
      e -> e
  in List.foldl f (Ok model) costs


applyReward : Currency.Bundle -> Model -> Model
applyReward reward =
  gain reward

applyRewards : List Currency.Bundle -> Model -> Model
applyRewards rewards model =
  List.foldl applyReward model rewards

map : (Currency.Bundle -> a) -> Model -> List a
map f model =
  Dict.toList model
    |> List.map Currency.bundleFromEnum
    |> List.map f
