module Inventory where

import Dict
import Html exposing (h3, text, div, ul, li, span)

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

view : Model -> Html.Html
view model = 
    div []
        [ h3 [] [text "Inventory"]
        , let lines = map viewCurrency model
            in ul [] lines
        ]

viewCurrency : Currency.Bundle -> Html.Html
viewCurrency (t, amount) =
    li []
        [ span [] [text <| toString t ++ ":"]
        , span [] [text <| Format.int amount]
        ]

get : Currency.Type -> Model -> Int
get t model =
    Dict.get (Currency.toEnum t) model
        |> Maybe.withDefault 0

set : Currency.Type -> Int -> Model -> Model
set t amount model =
    Dict.update (Currency.toEnum t) (\_ -> Just amount) model

gain : Currency.Type -> Int -> Model -> Model
gain t amount model =
    let cur = get t model
    in set t (cur + amount) model

spend : Currency.Type -> Int -> Model -> Result String Model
spend t cost model =
    let curAmt = get t model
        canAfford = curAmt >= cost
    in if canAfford then
            Ok <| set t (curAmt - cost) model
        else
            Err <| "Not enough " ++ toString t

applyReward : Currency.Bundle -> Model -> Model
applyReward (t, amount) =
    gain t amount

applyRewards : List Currency.Bundle -> Model -> Model
applyRewards rewards model =
    List.foldl applyReward model rewards

map : (Currency.Bundle -> a) -> Model -> List a
map f model =
    Dict.toList model
        |> List.map Currency.bundleFromEnum
        |> List.map f
