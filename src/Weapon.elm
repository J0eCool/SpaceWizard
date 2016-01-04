module Weapon where

type alias Model =
  { kind : Kind
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

init : Type -> Float -> Int -> Model
init t level id =
  let kind = typeToKind t
  in
    { kind = kind
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

name : Model -> String
name weapon =
  weapon.kind.name

damage : Model -> Int
damage weapon =
  let
    dmg = toFloat weapon.kind.damage
    lv = weapon.level - 1
    lvMod = 1 + 0.25 * lv
  in round <| dmg * lvMod

speed : Model -> Float
speed weapon =
  weapon.kind.attackSpeed
