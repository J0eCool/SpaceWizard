module Items.Enchant (..) where

import Focus
import Serialize


type Type
  = Damage
  | Speed
  | Beast


type alias Kind =
  { name : String
  , modifier : Modifier
  }


type alias Modifier =
  { attack : Float
  , speed : Float
  }


type alias Model =
  { kind : Kind
  , level : Float
  }


level : Focus.Focus Model Float
level =
  Focus.create .level (\f m -> { m | level = f m.level })


initWith : Type -> Model
initWith t =
  { kind = typeToKind t
  , level = 1
  }


allTypes : List Type
allTypes =
  [ Damage
  , Speed
  , Beast
  ]


typeToKind : Type -> Kind
typeToKind t =
  let
    base atk spd =
      { name = toString t
      , modifier =
          { attack = atk
          , speed = spd
          }
      }
  in
    case t of
      Damage ->
        base 0.3 0

      Speed ->
        base 0 0.2

      Beast ->
        base 0.1 0.1


allKinds : List Kind
allKinds =
  List.map typeToKind allTypes


cost : Model -> Int
cost model =
  let
    lv =
      model.level - 1
  in
    floor <| 100 * (0.5 * lv + 0.45 * lv * lv + 5.0e-2 * lv * lv * lv)


modifier : Model -> Modifier
modifier model =
  let
    lv =
      model.level

    mod =
      model.kind.modifier
  in
    { attack = mod.attack * lv
    , speed = mod.speed * lv
    }


serializer : Serialize.Serializer Model
serializer =
  Serialize.object2
    (initWith Damage)
    ( "level", level, Serialize.float )
    ( "kind"
    , Focus.create .kind (\f m -> { m | kind = f m.kind })
    , Serialize.namedStringList allKinds
    )
