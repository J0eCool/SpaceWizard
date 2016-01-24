module Map where

import Focus exposing ((=>))
import Html exposing (Html, div, span, h3, text, ul, li, button)
import Html.Events exposing (onClick)

import BattleStats
import Format
import ListUtil exposing (index, updateIndex, indexWith)
import Operators exposing ((?>))
import Serialize

type alias Model =
  { areas : List Area
  , selectedId : Int
  }

type alias Area =
  { name : String
  , stage : Int
  , highestStageBeaten : Int
  , enemy : EnemyType
  }

type alias EnemyType =
  { name : String
  , health : Float
  , attack : Float
  , speed : Float
  }

type Action
  = Select Int
  | Increment
  | Decrement
  | BeatStage

init : Model
init =
  { areas =
      [ { initArea
        | name = "Plains"
        , enemy =
            { name = "Imp"
            , health = 0.7
            , attack = 0.8
            , speed = 0.8
            }
        }
      , { initArea
        | name = "Forest"
        , enemy =
            { name = "Snake"
            , health = 0.6
            , attack = 0.8
            , speed = 1
            }
        }
      ]
  , selectedId =
      0
  }

initArea : Area
initArea =
  { name = "INVALID AREA"
  , stage = 1
  , highestStageBeaten = 0
  , enemy = initEnemy
  }

initEnemy : EnemyType
initEnemy =
  { name = "INVALID ENEMY"
  , health = 1
  , attack = 1
  , speed = 1
  }

selected : Model -> Area
selected model =
  index model.selectedId model.areas
    |> Maybe.withDefault initArea

enemyType : Model -> EnemyType
enemyType model =
  (selected model).enemy

enemyLevel : Model -> Int
enemyLevel model =
  let area = selected model
  in area.stage

enemyDerived : Model -> BattleStats.Derived
enemyDerived map =
  let
    area =
      selected map
    enemy =
      area.enemy
    l =
      toFloat <| area.stage - 1
  in
    { name = enemy.name
    , maxHealth =
        floor <| (100 + 18 * l + 2 * l ^ 2) * enemy.health
    , attackDamage =
        floor <| (10 + 3 * l + l ^ 2) * enemy.attack
    , armor =
        floor <| (l * 4 + 0.5 * l ^ 1.5)
    , healthRegen =
        2
    , attackSpeed =
        enemy.speed
    }

update : Action -> Model -> (Model, Bool)
update action model =
  case action of
    Select id ->
      ({ model | selectedId = id }, True)
    Increment ->
      ( updateSelected (\area ->
        { area
        | stage = min (area.highestStageBeaten + 1) (area.stage + 1)
        }) model
      , True
      )
    Decrement ->
      ( updateSelected (\area ->
        { area
        | stage = max 1 (area.stage - 1)
        }) model
      , True
      )
    BeatStage ->
      ( updateSelected (\area ->
        { area
        | highestStageBeaten = max area.highestStageBeaten area.stage
        }) model
      , False
      )

updateSelected : (Area -> Area) -> Model -> Model
updateSelected f model =
  { model
  | areas =
      updateIndex model.selectedId f model.areas
  }

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ h3 [] [text "Map"]
    , ul [] (List.indexedMap (viewArea address model.selectedId) model.areas)
    ]

viewArea : Signal.Address Action -> Int -> Int -> Area -> Html
viewArea address selectedId idx area =
  li [] <|
    [ text <| area.name ++ " (" ++ Format.int area.stage ++ ")"
    ]
    ++ if selectedId /= idx then [button [onClick address (Select idx)] [text "Select"]] else []

areaSerializer : Area -> Serialize.Serializer Area
areaSerializer area =
  let
    stage =
      Focus.create .stage (\f a -> { a | stage = f a.stage })
    highest =
      Focus.create .highestStageBeaten (\f a -> { a | highestStageBeaten = f a.highestStageBeaten })
  in
    Serialize.object2 area
      ("stage", stage, Serialize.int)
      ("highest", highest, Serialize.int)

serializer : Serialize.Serializer Model
serializer =
  let
    focusFor area =
      let
        idx =
          indexWith (\a -> a.name == area.name) init.areas
        getter model =
          idx ?> (\i -> index i model.areas)
            |> Maybe.withDefault initArea
        updater f model =
          case idx of
            Just i ->
              let areas = updateIndex i f model.areas
              in { model | areas = areas }
            Nothing ->
              model
      in
        Focus.create getter updater
    areaData area =
      (area.name, focusFor area, areaSerializer area)
    data =
      List.map areaData init.areas
  in
    Serialize.list data init
