module Widgets.LevelText (..) where

import Html exposing (Html, span, text)
import Format
import Style exposing (..)


view : Float -> Html
view level =
  let
    hi =
      floor level

    lo =
      (floor (100 * level)) % 100

    loStr =
      if lo < 10 then
        "0" ++ toString lo
      else
        toString lo
  in
    span
      []
      [ span
          [ style [ fontWeight Bold, fontSize (Px 16) ] ]
          [ text <| Format.int hi ]
      , span
          []
          [ text "." ]
      , span
          [ style [ fontSize (Px 10) ] ]
          [ text loStr ]
      ]
