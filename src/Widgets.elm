module Widgets (..) where

import Html exposing (Html, span, text, button, input)
import Html.Attributes exposing (type', checked, disabled)
import Html.Events exposing (onClick)


selectable : String -> Signal.Address action -> String -> action -> Bool -> Html
selectable kind address label action value =
  span
    []
    [ input
        [ type' kind
        , checked value
        , onClick address action
        ]
        []
    , text label
    ]


checkbox : Signal.Address action -> String -> action -> Bool -> Html
checkbox =
  selectable "checkbox"


radio : Signal.Address action -> String -> action -> Bool -> Html
radio =
  selectable "radio"
