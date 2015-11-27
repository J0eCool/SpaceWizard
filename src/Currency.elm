module Currency where

import Dict

type Type
    = Gold
    | Experience

toEnum : Type -> Int
toEnum t =
    case t of
        Gold -> 0
        Experience -> 1

type alias Inventory =
    Dict.Dict Int Int -- Dict Type Int

type alias Reward =
    (Type, Int)

emptyInventory : Inventory
emptyInventory =
    Dict.empty

get : Type -> Inventory -> Int
get t inventory =
    Dict.get (toEnum t) inventory
        |> Maybe.withDefault 0

set : Type -> Int -> Inventory -> Inventory
set t amount inventory =
    let f x = Just amount
    in Dict.update (toEnum t) f inventory

gain : Type -> Int -> Inventory -> Inventory
gain t amount inventory =
    let cur = get t inventory
    in set t (cur + amount) inventory

spend : Type -> Int -> Inventory -> (Inventory, Bool)
spend t cost inventory =
    let curAmt = get t inventory
        canAfford = curAmt >= cost
        inventory' =
            if canAfford then
                set t (curAmt - cost) inventory
            else
                inventory
    in (inventory', canAfford)

applyReward : Reward -> Inventory -> Inventory
applyReward (t, amount) =
    gain t amount

applyRewards : List Reward -> Inventory -> Inventory
applyRewards rewards inventory =
    List.foldl applyReward inventory rewards
