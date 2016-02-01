module Equipment (..) where

import Focus exposing (..)
import Html exposing (Html, div, span, h3, h4, text, button, ul, li)
import Html.Events exposing (onClick, onMouseDown, onMouseUp, onMouseEnter, onMouseLeave)
import Cost
import Currency
import Format
import ListUtil exposing (contains, remove, replace, mapSum)
import Serialize
import Style exposing (..)
import Weapon
import Widgets
import Widgets.UpgradeSlot as UpgradeSlot


type alias Model =
  { weapon : Weapon.Model
  , armor : Float
  , inventory : List Weapon.Model
  , selectedWeaponType : Weapon.Type
  , selectedWeaponMaterial : Currency.Type
  , nextId : Int
  , heldAction : Maybe TimedAction
  , upgradeVelocity : Float
  }


init : Model
init =
  { weapon = Weapon.init
  , armor = 1
  , inventory = []
  , selectedWeaponType = Weapon.Sword
  , selectedWeaponMaterial = Currency.Iron
  , nextId = 1
  , heldAction = Nothing
  , upgradeVelocity = 0
  }


type Action
  = NoOp
  | Tick Float
  | UpgradeAction (UpgradeSlot.Action TimedAction)
  | Equip Weapon.Model
  | Discard Weapon.Model
  | SelectType Weapon.Type
  | SelectMaterial Currency.Type
  | Craft


type TimedAction
  = Upgrade (Focus Model Weapon.Model)


attackDamage : Model -> Int
attackDamage model =
  Weapon.damage model.weapon


attackSpeed : Model -> Float
attackSpeed model =
  Weapon.speed model.weapon


armor : Model -> Int
armor model =
  let
    arm =
      model.armor - 1
  in
    round <| 5 + arm


update : Action -> Model -> ( Model, List Currency.Bundle )
update action model =
  let
    no m =
      ( m, [] )
  in
    case action of
      NoOp ->
        no model

      Equip weapon ->
        if model.weapon == weapon then
          no model
        else
          let
            removedInv =
              remove weapon model.inventory

            addedInv =
              removedInv ++ [ model.weapon ]
          in
            { model
              | weapon = weapon
              , inventory = addedInv
            }
              |> no

      Discard weapon ->
        no { model | inventory = remove weapon model.inventory }

      SelectType t ->
        no { model | selectedWeaponType = t }

      SelectMaterial mat ->
        no { model | selectedWeaponMaterial = mat }

      Craft ->
        let
          crafted =
            toCraft model
        in
          ( { model
              | inventory = model.inventory ++ [ crafted ]
              , nextId = model.nextId + 1
            }
          , Weapon.craftCost crafted
          )

      UpgradeAction action ->
        case action of
          UpgradeSlot.SetHeld act ->
            no { model | heldAction = Just act }

          UpgradeSlot.SetHover act ->
            no model

          UpgradeSlot.Release ->
            no { model | heldAction = Nothing, upgradeVelocity = 0 }

          UpgradeSlot.MoveOut ->
            update (UpgradeAction UpgradeSlot.Release) model

      Tick dT ->
        case model.heldAction of
          Nothing ->
            no model

          Just act ->
            updateTick dT act model


updateTick : Float -> TimedAction -> Model -> ( Model, List Currency.Bundle )
updateTick dT action model =
  case action of
    Upgrade focus ->
      let
        vel =
          model.upgradeVelocity + dT

        amt =
          vel * dT

        weapon =
          get focus model

        upgraded =
          { weapon | level = weapon.level + amt }

        upgradeCost =
          cost amt focus model

        tickedModel =
          { model | upgradeVelocity = vel }
      in
        ( set focus upgraded tickedModel, [ upgradeCost ] )


toCraft : Model -> Weapon.Model
toCraft model =
  Weapon.initWith model.selectedWeaponType model.selectedWeaponMaterial 1 model.nextId


inline =
  style [ display InlineBlock ]


inlineTop =
  style [ display InlineBlock, verticalAlign Top ]


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ div
        [ inline ]
        [ h3 [] [ text "Equipment" ]
        , div
            []
            [ text "Equipped weapon"
            , viewWeapon address (div []) model model.weapon
            ]
        , div [] [ text <| "Armor: " ++ Format.float model.armor ]
        ]
    , div
        [ inlineTop ]
        [ h3 [] [ text "Inventory" ]
        , ul [] <| List.map (viewWeapon address (li [ inline ]) model) model.inventory
        ]
    , viewCrafting address model
    ]


viewWeapon : Signal.Address Action -> (List Html -> Html) -> Model -> Weapon.Model -> Html
viewWeapon address elem model weapon =
  let
    focus =
      focusFor weapon model

    isEquipped =
      model.weapon == weapon

    forwarded =
      Signal.forwardTo address UpgradeAction

    upgradeContext =
      { title = always "Level"
      , level = .level
      , format = Format.currency
      , elem = li
      }

    upgradeButton =
      [ UpgradeSlot.viewStat upgradeContext cost Upgrade forwarded focus model ]

    inventoryActionButtons =
      if isEquipped then
        []
      else
        [ li
            []
            [ button [ onClick address <| Equip weapon ] [ text "Equip" ]
            ]
        , li
            []
            [ button
                [ onClick address <| Discard weapon ]
                [ text "Discard" ]
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
  in
    elem
      [ div [] [ text <| Weapon.name weapon ]
      , ul
          []
          ([ li [] [ text <| "Damage " ++ Format.int damage ]
           , li [] [ text <| "Attack Speed " ++ Format.float speed ++ "/s" ]
           , li [] [ text <| "DPS " ++ Format.float dps ]
           ]
            ++ buttons
          )
      ]


viewCrafting : Signal.Address Action -> Model -> Html
viewCrafting address model =
  let
    radio act cmp t =
      li [] [ Widgets.radio address (toString t) (act t) (cmp == t) ]

    weapon =
      toCraft model

    craftButton =
      button
        [ onClick address Craft ]
        [ text <| "Craft (" ++ Format.currencyList (Weapon.craftCost weapon) ++ ")" ]
  in
    div
      []
      [ h3 [] [ text "Crafting" ]
      , div
          [ inline ]
          [ text "Type"
          , ul [] <| List.map (radio SelectType model.selectedWeaponType) Weapon.allTypes
          ]
      , div
          [ inlineTop ]
          [ text "Material"
          , ul [] <| List.map (radio SelectMaterial model.selectedWeaponMaterial) Weapon.allMaterials
          ]
      , div
          [ inlineTop ]
          [ text "Result"
          , viewBaseWeapon (div []) [ craftButton ] weapon
          ]
      ]


equippedWeapon =
  create .weapon <| \f m -> { m | weapon = f m.weapon }


inventoryWeapon wep =
  create (always wep)
    <| \f m -> { m | inventory = replace wep (f wep) m.inventory }


focusFor weapon model =
  if model.weapon == weapon then
    equippedWeapon
  else
    inventoryWeapon weapon


inventory =
  create .inventory (\f m -> { m | inventory = f m.inventory })


level =
  create .level <| \f w -> { w | level = f w.level }


cost : Float -> Focus Model Weapon.Model -> Model -> Currency.Bundle
cost delta weapon model =
  ( Currency.Gold
  , Cost.cost totalCost delta (weapon => level) model
  )


totalCost : Model -> Int
totalCost model =
  let
    equippedCost =
      Weapon.cost model.weapon

    inventoryCost =
      mapSum Weapon.cost model.inventory

    totalCost =
      equippedCost + inventoryCost
  in
    totalCost


serializer : Serialize.Serializer Model
serializer =
  Serialize.object2
    init
    ( "weapon", equippedWeapon, Weapon.serializer )
    ( "inventory", inventory, Serialize.list Weapon.serializer )
