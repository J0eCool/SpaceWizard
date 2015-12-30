module Equipment where

import Html exposing (Html, div, span, h3, h4, text, button, ul, li)
import Html.Events exposing (onClick)

import Format
import ListUtil exposing (remove)

type alias Model =
  { weapon : Weapon
  , armor : Float
  , inventory : List Weapon
  }

type alias Weapon =
  { name : String
  , damage : Int
  , level : Float
  }

type Action
  = NoOp
  | Upgrade Weapon
  | Equip Weapon

init : Model
init =
  { weapon =
    { name = "Sword"
    , damage = 20
    , level = 1
    }
  , armor = 1
  , inventory =
      [ { name = "Dagger"
        , damage = 15
        , level = 1
        }
      , { name = "Axe"
        , damage = 26
        , level = 1
        }
      ]
  }

attackDamage : Model -> Int
attackDamage model =
  weaponDamage model.weapon

weaponDamage : Weapon -> Int
weaponDamage weapon =
  let
    dmg = toFloat weapon.damage
    lv = weapon.level - 1
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
    Upgrade weapon ->
      model
      --let wep = model.weapon
      --in { model | weapon = { wep | level = wep.level + 1 } }
    Equip weapon ->
      model

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h3 [] [text "Equipment"]
    , div []
      [ text "Equipped weapon"
      , viewWeapon address div model.weapon
      ]
    , div [] [text <| "Armor: " ++ Format.float model.armor]
    , h3 [] [text "Inventory"]
    , ul [] <| List.map (viewWeapon address li) model.inventory
    ]

viewWeapon : Signal.Address Action -> (List Html.Attribute -> List Html -> Html) -> Weapon -> Html
viewWeapon address elem weapon =
  elem []
    [ div []
        [ text
            <| weapon.name
            ++ " (lv "
            ++ Format.float weapon.level
            ++ ")"
        ]
    , ul []
        [ li [] [text <| "Damage " ++ Format.int (weaponDamage weapon)]
        , li []
            [ button [onClick address <| Equip weapon] [text "Equip"] ]
        , li []
            [ button [onClick address <| Upgrade weapon] [text "Upgrade"]]
        ]
    ]
