module Equipment where

import Html exposing (Html, div, h3, text, button)
import Html.Events exposing (onClick)

import Format

type alias Model =
  { weapon : Weapon
  , armor : Float
  }

type alias Weapon =
  { name : String
  , damage : Int
  , level : Float
  }

type Action
  = NoOp
  | UpgradeWeapon

init : Model
init =
  { weapon =
    { name = "Sword"
    , damage = 20
    , level = 1
    }
  , armor = 1
  }

attackDamage : Model -> Int
attackDamage model =
  let
    dmg = toFloat model.weapon.damage
    lv = model.weapon.level - 1
    lvMod = 1 + 0.25 * lv
  in round <| dmg * lvMod

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
      let wep = model.weapon
      in { model | weapon = { wep | level = wep.level + 1 } }

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h3 [] [text "Equipment"]
    , div []
      [ text <| "Weapon: " ++ Format.float model.weapon.level
      , button [onClick address UpgradeWeapon] [text "+"]
      ]
    , div [] [text <| "Armor: " ++ Format.float model.armor]
    ]