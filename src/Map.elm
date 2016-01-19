module Map where

import Html exposing (Html, div, span, h3, text, ul, li, button)
import Html.Events exposing (onClick)

import ListUtil exposing (index, updateIndex)

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

update : Action -> Model -> Model
update action model =
  case action of
    Select id ->
      { model | selectedId = id }
    Increment ->
      updateSelected (\area ->
        { area
        | stage = min (area.highestStageBeaten + 1) (area.stage + 1)
        }) model
    Decrement ->
      updateSelected (\area ->
        { area
        | stage = max 1 (area.stage - 1)
        }) model
    BeatStage ->
      updateSelected (\area ->
        { area
        | highestStageBeaten = max area.highestStageBeaten area.stage
        }) model

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
  li []
    [ text <| area.name ++ if selectedId == idx then "*" else ""
    , button [onClick address (Select idx)] [text "Select"]
    ]
