module Equipment where

type alias Model =
  { weapon : Float
  , armor : Float
  }

init : Model
init =
  { weapon = 1
  , armor = 1
  }

attackDamage : Model -> Int
attackDamage model =
  let wep = model.weapon - 1
  in round <| 20 + 5 * wep

armor : Model -> Int
armor model =
  let arm = model.armor - 1
  in round <| 5 + arm
