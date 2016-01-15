import Char exposing (KeyCode)
import Effects
import Html exposing (Html, span, div, button, text, h3, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode exposing (Value)
import Keyboard
import Signal
import StartApp
import String
import Time exposing (inSeconds, fps)

import Battle
import BattleStats
import Buildings
import Currency
import Equipment
import Format
import Inventory
import Keys

type alias Model =
  { inventory : Inventory.Model
  , battle : Battle.Model
  , stats : BattleStats.Model
  , equipment : Equipment.Model
  , buildings : Buildings.Model
  , activeMainTab : Tab
  , fakeValue : Int
  }

type Action
  = Tick Float
  | ChooseTab Tab
  | BattleAction Battle.Action
  | StatsAction BattleStats.Action
  | EquipAction Equipment.Action
  | BuildingAction Buildings.Action
  | KeyPress Keys.Key

type Tab
  = BattleTab
  | EquipmentTab
  | BuildingsTab

allTabs : List Tab
allTabs =
  [ BattleTab
  , EquipmentTab
  , BuildingsTab
  ]

type alias View =
  Signal.Address Action -> Model -> Html

main : Signal Html
main =
  app.html

app : StartApp.App Model
app =
  StartApp.start
    { init = (load getStorage init, Effects.none)
    , view = view
    , update = \a m -> (update a m, Effects.none)
    , inputs = inputs
    }

init : Model
init =
  let
    stats = BattleStats.init
    equip = Equipment.init
    battleContext =
      { stats = stats
      , equipment = equip
      }
  in
    { inventory = Inventory.init
    , battle = Battle.init battleContext
    , stats = stats
    , equipment = equip
    , buildings = Buildings.init
    , activeMainTab = BattleTab
    , fakeValue = 0
    }

inputs : List (Signal Action)
inputs =
  [ fps 60 |> Signal.map Tick
  , Keys.pressed |> Signal.map KeyPress
  ]

view : View
view address model =
  let
    fwd =
      Signal.forwardTo address
    viewTab tab =
      button [onClick address <| ChooseTab tab] [text <| .name <| tabData tab]
    tabs =
      List.map viewTab allTabs
  in div [] (
    tabs ++
    [ (tabData model.activeMainTab).view address model
    , lazy Inventory.view model.inventory
    , lazy2 (BattleStats.view <| fwd StatsAction) model.equipment model.stats
    , text <| "Fake: " ++ toString model.fakeValue
    ])

update : Action -> Model -> Model
update action model =
  case action of
    Tick delta ->
      let
        dT = inSeconds delta
        wrapped =
          [ BattleAction << Battle.Tick
          , StatsAction << BattleStats.Tick
          , BuildingAction << Buildings.Tick
          , EquipAction << Equipment.Tick
          ]
        actions =
          List.map (\f -> update <| f dT) wrapped
      in List.foldl (\f m -> f m) model actions
    ChooseTab tab ->
      { model | activeMainTab = tab }
    BattleAction bAction ->
      let
        (battle', battleRewards) =
          Battle.update bAction model model.battle
      in
        { model
        | battle = battle'
        , inventory = Inventory.applyRewards battleRewards model.inventory
        }
    StatsAction sAction ->
      let
        statUpdate =
          BattleStats.update sAction model.stats
        statSetter s m =
          { m | stats = s }
      in tryPurchase statSetter statUpdate model
    EquipAction eAction ->
      let
        equipUpdate =
          Equipment.update eAction model.equipment
        equipSetter e m =
          { m | equipment = e }
      in tryPurchase equipSetter equipUpdate model
    BuildingAction action ->
      let
        (updatedBuildings, effects) =
          Buildings.update action model.buildings
        buildingSetter b m =
          { m | buildings = b }
      in
        { model
        | inventory = Inventory.applyFloatRewards effects.reward model.inventory
        }
        |> tryPurchase buildingSetter (updatedBuildings, effects.cost)
    KeyPress key ->
      let
        battle' =
          Battle.update
            (Battle.KeyPress key)
            model
            model.battle
            |> fst
        fv =
          model.fakeValue +
            case key of
              Keys.KeyChar '+' -> 1
              Keys.KeyChar '-' -> -1
              _ -> 0
      in { model | battle = battle', fakeValue = fv }

encode : Model -> Value
encode model =
  Encode.object
    [ ("fakeValue", Encode.int model.fakeValue)
    ]

decodeModel : Int -> Model
decodeModel val =
  { init | fakeValue = val }

decoder : Decode.Decoder Model
decoder =
  Decode.object1 decodeModel
    ("fakeValue" := Decode.int)

load : Maybe String -> Model -> Model
load storage model =
  case storage of
    Nothing ->
      model
    Just save ->
      case String.toInt save of
        Err _ ->
          model
        Ok val ->
          { model | fakeValue = val }

tabData : Tab -> { name : String, view : View }
tabData tab =
  case tab of
    BattleTab ->
      { name = "Battle"
      , view = \address model ->
          Battle.view (Signal.forwardTo address BattleAction) model model.battle
      }
    EquipmentTab ->
      { name = "Equipment"
      , view = \address model ->
          lazy (Equipment.view <| Signal.forwardTo address EquipAction) model.equipment
      }
    BuildingsTab ->
      { name = "Buildings"
      , view = \address model ->
          lazy (Buildings.view <| Signal.forwardTo address BuildingAction) model.buildings
      }

tryPurchase : (a -> Model -> Model) -> (a, List Currency.Bundle) -> Model -> Model
tryPurchase setter (updated, cost) model =
  let result =
    Inventory.spendAll cost model.inventory
  in case result of
    Ok updatedInventory ->
      setter updated { model | inventory = updatedInventory }
    Err _ ->
      model

port getStorage : Maybe String

port setStorage : Signal String
port setStorage =
  Signal.map (.fakeValue >> toString) app.model
    |> Signal.sampleOn (Time.every <| 1 * Time.second)
