module Currency (..) where

import Maybe
import ListUtil


type Type
  = Invalid
  | Experience
  | Gold
  | Iron
  | Aluminum
  | Steel


type alias Bundle =
  ( Type, Int )


type alias FloatBundle =
  ( Type, Float )


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

    Experience ->
      { abbreviation = "EXP"
      , enum = 0
      }

    Gold ->
      { abbreviation = "G"
      , enum = 1
      }

    Iron ->
      { abbreviation = "Ir"
      , enum = 2
      }

    Aluminum ->
      { abbreviation = "Al"
      , enum = 3
      }

    Steel ->
      { abbreviation = "St"
      , enum = 4
      }


allTypes : List Type
allTypes =
  [ Gold
  , Experience
  , Iron
  , Aluminum
  , Steel
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
    pred t =
      (toKind t).enum == e

    found =
      ListUtil.findWith pred allTypes
  in
    case found of
      Just t ->
        t

      Nothing ->
        Invalid


bundleToEnum : ( Type, a ) -> ( Int, a )
bundleToEnum ( t, packed ) =
  ( toEnum t, packed )


bundleFromEnum : ( Int, a ) -> ( Type, a )
bundleFromEnum ( enum, packed ) =
  ( fromEnum enum, packed )


bundleMap : (Int -> a) -> Bundle -> ( Type, a )
bundleMap f ( t, amt ) =
  ( t, f amt )
