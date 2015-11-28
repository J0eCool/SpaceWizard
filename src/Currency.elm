module Currency where

import Maybe

import ListUtil

type Type
    = Invalid -- Used for default for to/fromEnum
    | Gold
    | Experience

type alias Bundle =
    (Type, Int)

enumPairing : List (Type, Int)
enumPairing =
    [ (Invalid, -1)
    , (Gold, 0)
    , (Experience, 1)
    ]

toEnum : Type -> Int
toEnum t =
    let pred (fType, _) = fType == t
        found = ListUtil.findWith pred enumPairing
    in case found of
        Just (_, fEnum) ->
            fEnum
        Nothing ->
            -1

fromEnum : Int -> Type
fromEnum e =
    let pred (_, fEnum) = fEnum == e
        found = ListUtil.findWith pred enumPairing
    in case found of
        Just (fType, _) ->
            fType
        Nothing ->
            Invalid

bundleToEnum : Bundle -> (Int, Int)
bundleToEnum (t, amount) = (toEnum t, amount)

bundleFromEnum : (Int, Int) -> Bundle
bundleFromEnum (enum, amount) = (fromEnum enum, amount)
