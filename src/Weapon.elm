module Weapon where

import Cost
import Currency

type alias Model =
  { kind : Kind
  , material : MaterialKind
  , level : Float
  , id : Int
  }

type alias Kind =
  { name : String
  , damage : Int
  , attackSpeed : Float
  }

type Type
  = Sword
  | Dagger
  | Axe

allTypes : List Type
allTypes =
  [ Sword
  , Dagger
  , Axe
  ]

type alias MaterialKind =
  { name : String
  , currency : Currency.Type
  , damage : Float
  , speed : Float
  , cost : Float
  , goldCost : Float
  }

init : Type -> Currency.Type -> Float -> Int -> Model
init t mat level id =
  let
    kind = typeToKind t
    matKind = currencyToMaterial mat
  in
    { kind = kind
    , material = matKind
    , level = level
    , id = id
    }

typeToKind : Type -> Kind
typeToKind t =
  let
    base damage speed =
      { name = toString t
      , damage = damage
      , attackSpeed = speed
      }
  in case t of
    Sword ->
      base 20 1.2
    Dagger ->
      base 15 1.5
    Axe ->
      base 26 1

currencyToMaterial : Currency.Type -> MaterialKind
currencyToMaterial currency =
  let
    base damage speed cost goldCost =
      { name = toString currency
      , currency = currency
      , damage = damage
      , speed = speed
      , cost = cost
      , goldCost = goldCost
      }
  in case currency of
    Currency.Iron ->
      base 1 1 10 15
    Currency.Aluminum ->
      base 0.9 1.3 25 50
    Currency.Steel ->
      base 1.4 1 75 200
    _ ->
      let invalid = base 0 0 0 0
      in { invalid | name = "INVALID" }

allMaterials : List Currency.Type
allMaterials =
  List.filter (\c -> (currencyToMaterial c).name /= "INVALID") Currency.allTypes

name : Model -> String
name weapon =
  weapon.material.name ++ " " ++ weapon.kind.name

damage : Model -> Int
damage weapon =
  let
    dmg = toFloat weapon.kind.damage
    lv = weapon.level - 1
    lvMod = 1 + 0.25 * lv
    matMod = weapon.material.damage
  in round <| dmg * lvMod * matMod

speed : Model -> Float
speed weapon =
  let
    spd = weapon.kind.attackSpeed
    matMod = weapon.material.speed
  in spd * matMod

craftCost : Model -> List Currency.Bundle
craftCost weapon =
  let
    matCost =
      weapon.material.cost * 10
    goldCost =
      weapon.material.goldCost * 20
  in
    [ (weapon.material.currency, matCost)
    , (Currency.Gold, goldCost)
    ]

cost : Model -> Int
cost wep =
  round <| Cost.base (2, 1, 2) 1.10 wep.level * wep.material.goldCost / 15