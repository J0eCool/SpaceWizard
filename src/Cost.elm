module Cost (..) where

import Focus exposing (..)


cost : (a -> Int) -> Float -> Focus a Float -> a -> Int
cost totalCost delta focus model =
    let
        cur = totalCost model

        nextModel = update focus ((+) delta) model

        next = totalCost nextModel
    in
        next - cur


base : ( Float, Float, Float ) -> Float -> Float -> Float
base ( a, b, c ) power level =
    -- (ax^3 + bx^2 + cx) * power ^ level
    level * (c + level * (b + level * a)) * (power ^ level)
