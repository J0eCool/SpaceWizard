module Equipment where

import Html exposing (Html, div, h3, text, button)
import Html.Events exposing (onClick)

import Format

type alias Model =
  { weapon : Float
  , armor : Float
  }

type Action
  = NoOp
  | UpgradeWeapon

init : Model
init =
  { weapon = 1
  , armor = 1
  }

attackDamage : Model -> Int
attackDamage model =
  let wep = model.weapon - 1
  in round <| 20 + 5 * wep

armor : Model -> Int
armor model =
  let arm = model.armor - 1
  in round <| 5 + arm

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    UpgradeWeapon ->
      { model | weapon = model.weapon + 1 }

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h3 [] [text "Equipment"]
    , div []
      [ text <| "Weapon: " ++ Format.float model.weapon
      , button [onClick address UpgradeWeapon] [text "+"]
      ]
    , div [] [text <| "Armor: " ++ Format.float model.armor]
    ]