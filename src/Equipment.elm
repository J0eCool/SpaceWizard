module Equipment where

import Focus exposing (..)
import Html exposing (Html, div, span, h3, h4, text, button, ul, li)
import Html.Events exposing (onClick)

import Cost
import Currency
import Format
import ListUtil exposing (contains, remove, replaceFirst, mapSum)

type alias Model =
  { weapon : Weapon
  , armor : Float
  , inventory : List Weapon
  }

type alias Weapon =
  { name : String
  , damage : Int
  , attackSpeed : Float
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
    , attackSpeed = 1.2
    , level = 1
    }
  , armor = 1
  , inventory =
      [ { name = "Dagger"
        , damage = 15
        , attackSpeed = 1.5
        , level = 1
        }
      , { name = "Axe"
        , damage = 26
        , attackSpeed = 1.0
        , level = 1
        }
      ]
  }

weaponInit : Weapon
weaponInit =
  { name = "DEFAULT"
  , damage = 1
  , attackSpeed = 1
  , level = 1
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

attackSpeed : Model -> Float
attackSpeed model =
  model.weapon.attackSpeed

armor : Model -> Int
armor model =
  let arm = model.armor - 1
  in round <| 5 + arm

update : Action -> Model -> (Model, List Currency.Bundle)
update action model =
  let no m = (m, [])
  in case action of
    NoOp ->
      no model
    Upgrade weapon ->
      let
        upgraded =
          { weapon | level = weapon.level + 1 }
        focus =
          focusFor weapon model
        upgradeCost =
          cost 1 focus model
      in
        (set focus upgraded model, [upgradeCost])
    Equip weapon ->
      if model.weapon == weapon then
        no model
      else
        let
          removedInv =
            remove weapon model.inventory
          addedInv =
            removedInv ++ [model.weapon]
        in
          { model
          | weapon = weapon
          , inventory = addedInv
          } |> no

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h3 [] [text "Equipment"]
    , div []
      [ text "Equipped weapon"
      , viewWeapon address div model model.weapon
      ]
    , div [] [text <| "Armor: " ++ Format.float model.armor]
    , h3 [] [text "Inventory"]
    , ul [] <| List.map (viewWeapon address li model) model.inventory
    ]

viewWeapon : Signal.Address Action -> (List Html.Attribute -> List Html -> Html) ->
  Model -> Weapon -> Html
viewWeapon address elem model weapon =
  let
    focus =
      focusFor weapon model
    upgradeCost =
      cost 1 focus model
  in elem []
    [ div []
        [ text
            <| weapon.name
            ++ " (lv "
            ++ Format.float weapon.level
            ++ ")"
        ]
    , ul []
        [ li [] [text <| "Damage " ++ Format.int (weaponDamage weapon)]
        , li [] [text <| "Attack Speed " ++ Format.float weapon.attackSpeed ++ "/s"]
        , li []
            [ button [onClick address <| Equip weapon] [text "Equip"] ]
        , li []
            [ button [onClick address <| Upgrade weapon] [text <| "Upgrade (" ++ Format.currency upgradeCost ++ ")"]]
        ]
    ]

equippedWeapon = create .weapon <| \f m -> { m | weapon = f m.weapon }
inventoryWeapon wep =
  create (always wep)
    <| \f m -> { m | inventory = replaceFirst wep (f wep) m.inventory }
focusFor weapon model =
  if model.weapon == weapon then
    equippedWeapon
  else
    inventoryWeapon weapon

level = create .level <| \f w -> { w | level = f w.level}

cost : Float -> Focus Model Weapon -> Model -> Currency.Bundle
cost delta weapon model =
  Cost.cost totalCost delta (weapon => level) model

totalCost : Model -> Currency.Bundle
totalCost model =
  let
    cost wep =
      Cost.base (2, 1, 2) wep.level
    equippedCost =
      cost model.weapon
    inventoryCost =
      mapSum cost model.inventory
    totalCost =
      equippedCost + inventoryCost
  in
    ( Currency.Gold
    , floor totalCost
    )
