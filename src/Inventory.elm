module Inventory where

import Dict
import Html exposing (Html, h3, text, div, ul, li, span)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode exposing (Value)

import Currency
import Format

type alias Model =
  Dict.Dict Int Partial -- Dict Type Int

type alias Partial =
  { value : Int
  , part : Float
  }

type alias Bundle =
  (Currency.Type, Partial)

init : Model
init =
  [ (Currency.Gold, initPartial 0)
  , (Currency.Experience, initPartial 0)
  ]
    |> List.map Currency.bundleToEnum
    |> Dict.fromList

initPartial : Int -> Partial
initPartial value =
  { value = value
  , part = 0
  }

view : Model -> Html
view model =
  div []
    [ h3 [] [text "Currency"]
    ,   let lines = map viewCurrency model
      in ul [] lines
    ]

viewCurrency : Bundle -> Html
viewCurrency (t, partial) =
  li []
    [ span [] [text <| toString t ++ ":"]
    , span [] [text <| Format.int partial.value]
    ]

get : Currency.Type -> Model -> Int
get t model =
  Dict.get (Currency.toEnum t) model
    |> Maybe.withDefault (initPartial 0)
    |> .value

update : Currency.Type -> (Maybe Partial -> Maybe Partial) -> Model -> Model
update t f model =
  Dict.update (Currency.toEnum t) f model

gain : Currency.Bundle -> Model -> Model
gain (t, delta) model =
  let
    f partial =
      Just <| case partial of
        Just p ->
          { p | value = p.value + delta }
        Nothing ->
          { value = delta, part = 0 }
  in update t f model

gainFloat : Currency.FloatBundle -> Model -> Model
gainFloat (t, delta) model =
  let
    f partial =
      Just <| case partial of
        Just p ->
          let
            updated =
              p.part + delta
            toAdd =
              floor updated
          in { value = p.value + toAdd, part = updated - toFloat toAdd }
        Nothing ->
          let intVal = floor delta
          in { value = intVal, part = delta - toFloat intVal }
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

map : (Bundle -> b) -> Model -> List b
map f model =
  Dict.toList model
    |> List.map Currency.bundleFromEnum
    |> List.map f

encode : Model -> Value
encode model =
  Dict.toList model
    |> List.map (\(k, v) -> Encode.list [Encode.int k, Encode.int v.value])
    |> Encode.list

decoder : Decode.Decoder Model
decoder =
  Decode.tuple2 (\k v -> (k, initPartial v)) Decode.int Decode.int
    |> Decode.list
    |> Decode.map Dict.fromList
