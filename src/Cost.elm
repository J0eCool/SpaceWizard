module Cost where

import Focus exposing (..)

import Currency

cost : (a -> Currency.Bundle) -> Float -> Focus a Float -> a -> Currency.Bundle
cost totalCost delta focus model =
  let
    (currency, cur) = totalCost model
    nextModel = update focus ((+) delta) model
    (_, next) = totalCost nextModel
  in
    ( currency
    , next - cur
    )

base : (Float, Float, Float) -> Float -> Float
base (a, b, c) level =
  -- ax^3 + bx^2 + cx
  level * (c + level * (b + level * a))
