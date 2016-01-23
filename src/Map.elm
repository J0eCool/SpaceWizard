module Map where

import Focus exposing ((=>))
import Html exposing (Html, div, span, h3, text, ul, li, button)
import Html.Events exposing (onClick)

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
  }

type Action
  = Select Int
  | Increment
  | Decrement
  | BeatStage

init : Model
init =
  { areas =
      [ { initArea | name = "Plains" }
      , { initArea | name = "Forest" }
      ]
  , selectedId =
      0
  }

initArea : Area
initArea =
  { name = "INVALID AREA"
  , stage = 1
  , highestStageBeaten = 0
  }

selected : Model -> Area
selected model =
  index model.selectedId model.areas
    |> Maybe.withDefault initArea

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

serializer : Serialize.Serializer Model
serializer =
  let
    focusFor area =
      let
        idx =
          indexWith (\a -> a.name == area.name) init.areas
        toTuple a =
          (a.stage, a.highestStageBeaten)
        fromTuple (s, high) =
          { area | stage = s, highestStageBeaten = high }
        getter model =
          idx ?> (\i -> index i model.areas)
            |> Maybe.withDefault initArea
            |> toTuple
        updater f model =
          case idx of
            Just i ->
              let areas = updateIndex i (toTuple >> f >> fromTuple) model.areas
              in { model | areas = areas }
            Nothing ->
              model
      in
        Focus.create getter updater
    areaData area =
      (area.name, focusFor area, Serialize.tuple2 Serialize.int Serialize.int)
    data =
      List.map areaData init.areas
  in
    Serialize.list data init
