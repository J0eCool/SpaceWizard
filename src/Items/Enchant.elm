module Items.Enchant (..) where

import Focus
import Serialize


type Type
  = Damage
  | Speed


type alias Kind =
  { name : String
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
  , level = 0
  }


allTypes : List Type
allTypes =
  [ Damage
  , Speed
  ]


typeToKind : Type -> Kind
typeToKind t =
  { name = toString t }


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


serializer : Serialize.Serializer Model
serializer =
  Serialize.object2
    (initWith Damage)
    ( "level", level, Serialize.float )
    ( "kind"
    , Focus.create .kind (\f m -> { m | kind = f m.kind })
    , Serialize.namedStringList allKinds
    )
