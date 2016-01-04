module Weapon where

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
  , damage : Float
  , speed : Float
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
    base damage speed =
      { name = toString currency
      , damage = damage
      , speed = speed
      }
  in case currency of
    Currency.Iron ->
      base 1 1
    Currency.Aluminum ->
      base 0.9 1.3
    Currency.Steel ->
      base 1.4 1
    _ ->
      let invalid = base 0 0
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
