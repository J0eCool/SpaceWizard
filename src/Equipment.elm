module Equipment where

import Focus exposing (..)
import Html exposing (Html, div, span, h3, h4, text, button, ul, li)
import Html.Events exposing (onClick)

import Cost
import Currency
import Format
import ListUtil exposing (contains, remove, replace, mapSum)
import Weapon
import Widgets

type alias Model =
  { weapon : Weapon.Model
  , armor : Float
  , inventory : List Weapon.Model
  , selectedWeaponType : Weapon.Type
  , nextId : Int
  }

init : Model
init =
  { weapon = weaponInit
  , armor = 1
  , inventory = []
  , selectedWeaponType = Weapon.Sword
  , nextId = 1
  }

weaponInit : Weapon.Model
weaponInit =
  Weapon.init Weapon.Sword 1 0

type Action
  = NoOp
  | Upgrade Weapon.Model
  | Equip Weapon.Model
  | Discard Weapon.Model
  | Select Weapon.Type
  | Craft

attackDamage : Model -> Int
attackDamage model =
  Weapon.damage model.weapon

attackSpeed : Model -> Float
attackSpeed model =
  Weapon.speed model.weapon

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
    Discard weapon ->
      no { model | inventory = remove weapon model.inventory }
    Select t ->
      no { model | selectedWeaponType = t }
    Craft ->
      let
        new =
          Weapon.init model.selectedWeaponType 1 model.nextId
      in no
        { model
        | inventory = model.inventory ++ [new]
        , nextId = model.nextId + 1
        }

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
    , h3 [] [text "Crafting"]
    , viewCrafting address model
    ]

viewWeapon : Signal.Address Action -> (List Html.Attribute -> List Html -> Html) ->
  Model -> Weapon.Model -> Html
viewWeapon address elem model weapon =
  let
    focus =
      focusFor weapon model
    upgradeCost =
      cost 1 focus model
    isEquipped =
      model.weapon == weapon
    inventoryActionButtons =
      if isEquipped then
        []
      else
        [ li []
          [ button [onClick address <| Equip weapon]
            [text "Equip"] ]
        , li []
          [ button [onClick address <| Discard weapon]
            [text "Discard"]]
        ]
  in elem []
    [ div []
        [ text
            <| Weapon.name weapon
            ++ " (lv "
            ++ Format.float weapon.level
            ++ ")"
        ]
    , ul [] (
        [ li [] [text <| "Damage " ++ Format.int (Weapon.damage weapon)]
        , li [] [text <| "Attack Speed " ++ Format.float (Weapon.speed weapon) ++ "/s"]
        , li []
          [ button [onClick address <| Upgrade weapon]
            [text <| "Upgrade (" ++ Format.currency upgradeCost ++ ")"]]
        ]
        ++ inventoryActionButtons
      )
    ]

viewCrafting : Signal.Address Action -> Model -> Html
viewCrafting address model =
  let
    radio t =
      li [] [Widgets.radio address (toString t) (Select t) (model.selectedWeaponType == t)]
  in div []
    [ span [] [text "Type"]
    , ul [] <| List.map radio Weapon.allTypes
    , button [onClick address Craft] [text "Craft"]
    ]

equippedWeapon = create .weapon <| \f m -> { m | weapon = f m.weapon }
inventoryWeapon wep =
  create (always wep)
    <| \f m -> { m | inventory = replace wep (f wep) m.inventory }
focusFor weapon model =
  if model.weapon == weapon then
    equippedWeapon
  else
    inventoryWeapon weapon

level = create .level <| \f w -> { w | level = f w.level}

cost : Float -> Focus Model Weapon.Model -> Model -> Currency.Bundle
cost delta weapon model =
  Cost.cost totalCost delta (weapon => level) model

totalCost : Model -> Currency.Bundle
totalCost model =
  let
    cost wep =
      round <| Cost.base (2, 1, 2) 1.10 wep.level
    equippedCost =
      cost model.weapon
    inventoryCost =
      mapSum cost model.inventory
    totalCost =
      equippedCost + inventoryCost
  in
    ( Currency.Gold
    , totalCost
    )
