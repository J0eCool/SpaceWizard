module Equipment where

import Focus exposing (..)
import Html exposing (Html, div, span, h3, h4, text, button, ul, li)
import Html.Events exposing (onClick)

import Cost
import Currency
import Format
import ListUtil exposing (contains, remove, replace, mapSum)
import Style exposing (..)
import Weapon
import Widgets

type alias Model =
  { weapon : Weapon.Model
  , armor : Float
  , inventory : List Weapon.Model
  , selectedWeaponType : Weapon.Type
  , selectedWeaponMaterial : Currency.Type
  , nextId : Int
  }

init : Model
init =
  { weapon = weaponInit
  , armor = 1
  , inventory = []
  , selectedWeaponType = Weapon.Sword
  , selectedWeaponMaterial = Currency.Iron
  , nextId = 1
  }

weaponInit : Weapon.Model
weaponInit =
  Weapon.init Weapon.Sword Currency.Iron 1 0

type Action
  = NoOp
  | Upgrade Weapon.Model
  | Equip Weapon.Model
  | Discard Weapon.Model
  | SelectType Weapon.Type
  | SelectMaterial Currency.Type
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
    SelectType t ->
      no { model | selectedWeaponType = t }
    SelectMaterial mat ->
      no { model | selectedWeaponMaterial = mat }
    Craft ->
      let crafted = toCraft model
      in
        ( { model
          | inventory = model.inventory ++ [crafted]
          , nextId = model.nextId + 1
          }
        , Weapon.craftCost crafted
        )

toCraft : Model -> Weapon.Model
toCraft model =
  Weapon.init model.selectedWeaponType model.selectedWeaponMaterial 1 model.nextId

inline = style [display InlineBlock]
inlineTop = style [display InlineBlock, verticalAlign Top]

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [inline]
      [ h3 [] [text "Equipment"]
      , div []
        [ text "Equipped weapon"
        , viewWeapon address (div []) model model.weapon
        ]
      , div [] [text <| "Armor: " ++ Format.float model.armor]
      ]
    , div [inlineTop]
      [ h3 [] [text "Inventory"]
      , ul [] <| List.map (viewWeapon address (li [inline]) model) model.inventory
      ]
    , viewCrafting address model
    ]

viewWeapon : Signal.Address Action -> (List Html -> Html) ->
  Model -> Weapon.Model -> Html
viewWeapon address elem model weapon =
  let
    focus =
      focusFor weapon model
    upgradeCost =
      cost 1 focus model
    isEquipped =
      model.weapon == weapon
    upgradeButton =
      [ li []
          [ button [onClick address <| Upgrade weapon]
            [text <| "Upgrade (" ++ Format.currency upgradeCost ++ ")"]
          ]
      ]
    inventoryActionButtons =
      if isEquipped then
        []
      else
        [ li []
          [ button [onClick address <| Equip weapon]
            [text "Equip"]
          ]
        , li []
          [ button [onClick address <| Discard weapon]
            [text "Discard"]
          ]
        ]
    buttons =
      upgradeButton ++ inventoryActionButtons
  in
    viewBaseWeapon elem buttons weapon

viewBaseWeapon : (List Html -> Html) -> List Html -> Weapon.Model -> Html
viewBaseWeapon elem buttons weapon =
  let
    damage =
      Weapon.damage weapon
    speed =
      Weapon.speed weapon
    dps =
      toFloat damage * speed
  in elem
    [ div []
        [ text
            <| Weapon.name weapon
            ++ " (lv "
            ++ Format.float weapon.level
            ++ ")"
        ]
    , ul [] (
        [ li [] [text <| "Damage " ++ Format.int damage]
        , li [] [text <| "Attack Speed " ++ Format.float speed ++ "/s"]
        , li [] [text <| "DPS " ++ Format.float dps]
        ]
        ++ buttons
      )
    ]

viewCrafting : Signal.Address Action -> Model -> Html
viewCrafting address model =
  let
    radio act cmp t =
      li [] [Widgets.radio address (toString t) (act t) (cmp == t)]
    weapon =
      toCraft model
  in div []
    [ h3 [] [text "Crafting"]
    , div [inline]
      [ text "Type"
      , ul [] <| List.map (radio SelectType model.selectedWeaponType) Weapon.allTypes
      ]
    , div [inlineTop]
      [ text "Material"
      , ul [] <| List.map (radio SelectMaterial model.selectedWeaponMaterial) Weapon.allMaterials
      ]
    , div [inlineTop]
      [ text "Result"
      , viewBaseWeapon (div []) [] weapon
      ]
    , button [onClick address Craft] [text <| "Craft (" ++ Format.currencyList (Weapon.craftCost weapon) ++ ")"]
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
    equippedCost =
      Weapon.cost model.weapon
    inventoryCost =
      mapSum Weapon.cost model.inventory
    totalCost =
      equippedCost + inventoryCost
  in
    ( Currency.Gold
    , totalCost
    )
