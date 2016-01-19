module Inventory where

import Dict
import Focus
import Html exposing (Html, h3, text, div, ul, li, span)

import Currency
import Format
import Serialize

type alias Model =
  Dict.Dict Int Float -- Dict Type Float

init : Model
init =
  [ Currency.Gold
  , Currency.Experience
  ]
    |> List.map (\t -> (t, 0))
    |> List.map Currency.bundleToEnum
    |> Dict.fromList

view : Model -> Html
view model =
  div []
    [ h3 [] [text "Currency"]
    ,   let lines = map viewCurrency model
      in ul [] lines
    ]

viewCurrency : Currency.FloatBundle -> Html
viewCurrency (t, value) =
  li []
    [ span [] [text <| toString t ++ ":"]
    , span [] [text <| Format.int <| floor value]
    ]

get : Currency.Type -> Model -> Int
get t model =
  Dict.get (Currency.toEnum t) model
    |> Maybe.withDefault 0
    |> floor

update : Currency.Type -> (Maybe Float -> Maybe Float) -> Model -> Model
update t f model =
  Dict.update (Currency.toEnum t) f model

gain : Currency.Bundle -> Model -> Model
gain (t, delta) model =
  gainFloat (t, toFloat delta) model

gainFloat : Currency.FloatBundle -> Model -> Model
gainFloat (t, delta) model =
  let
    f amount =
      Just <| case amount of
        Just a ->
          a + delta
        Nothing ->
          delta
  in update t f model

spend : Currency.Bundle -> Model -> Result String Model
spend (t, cost) model =
  let
    curAmt = get t model
    canAfford = curAmt >= cost
  in  if canAfford then
      Ok <| gain (t, -cost) model
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

applyRewards : List Currency.Bundle -> Model -> Model
applyRewards rewards model =
  List.foldl gain model rewards

applyFloatRewards : List Currency.FloatBundle -> Model -> Model
applyFloatRewards rewards model =
  List.foldl gainFloat model rewards

map : (Currency.FloatBundle -> b) -> Model -> List b
map f model =
  Dict.toList model
    |> List.map Currency.bundleFromEnum
    |> List.map f

foucsFor : Currency.Type -> Focus.Focus Model Int
foucsFor t =
  Focus.create (get t) (\f m -> update t (Maybe.map (toFloat << f << floor)) m)

serializer : Serialize.Serializer Model
serializer =
  let
    toData t =
      (toString t, foucsFor t, Serialize.int)
    data =
      List.map toData Currency.allTypes
  in
    Serialize.list data init
