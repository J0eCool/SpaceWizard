module Currency where

import Maybe

import ListUtil

type Type
  = Invalid -- Used for default for to/fromEnum
  | Gold
  | Experience
  | Iron

type alias Bundle =
  (Type, Int)

type alias FloatBundle =
  (Type, Float)

type alias Kind =
  { abbreviation : String
  , enum : Int
  }

toKind : Type -> Kind
toKind t =
  case t of
    Invalid ->
      { abbreviation = "INVALID"
      , enum = -1
      }
    Gold ->
      { abbreviation = "G"
      , enum = 0
      }
    Experience ->
      { abbreviation = "EXP"
      , enum = 1
      }
    Iron ->
      { abbreviation = "Ir"
      , enum = 2
      }

allTypes : List Type
allTypes =
  [ Gold
  , Experience
  , Iron
  ]

abbreviation : Type -> String
abbreviation t =
  (toKind t).abbreviation

toEnum : Type -> Int
toEnum t =
  (toKind t).enum

fromEnum : Int -> Type
fromEnum e =
  let
    pred t = (toKind t).enum == e
    found = ListUtil.findWith pred allTypes
  in case found of
    Just t ->
      t
    Nothing ->
      Invalid

bundleToEnum : Bundle -> (Int, Int)
bundleToEnum (t, amount) =
  (toEnum t, amount)

bundleFromEnum : (Int, Int) -> Bundle
bundleFromEnum (enum, amount) =
  (fromEnum enum, amount)

bundleMap : (Int -> a) -> Bundle -> (Type, a)
bundleMap f (t, amt) =
  (t, f amt)
