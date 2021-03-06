module Main (..) where

import Char exposing (KeyCode)
import Effects
import Focus
import Html exposing (Html, span, div, button, text, h3, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Decode
import Json.Encode as Encode
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
import Mana
import Map
import MaybeUtil exposing (..)
import Serialize
import Style exposing (..)


type alias Model =
  { inventory : Inventory.Model
  , battle : Battle.Model
  , stats : BattleStats.Model
  , equipment : Equipment.Model
  , buildings : Buildings.Model
  , map : Map.Model
  , mana : Mana.Model
  , activeMainTab : Tab
  , loadError : Maybe String
  , shouldSave : Bool
  , shouldClearSave : Bool
  }


type Action
  = NoOp
  | Tick Float
  | ChooseTab Tab
  | BattleAction Battle.Action
  | StatsAction BattleStats.Action
  | EquipAction Equipment.Action
  | BuildingAction Buildings.Action
  | MapAction Map.Action
  | ManaAction Mana.Action
  | KeyPress Keys.Key
  | ClearSave
  | DidClearSave Bool


type Tab
  = BattleTab
  | EquipmentTab
  | BuildingsTab
  | MapTab


allTabs : List Tab
allTabs =
  [ BattleTab
  , EquipmentTab
  , BuildingsTab
  , MapTab
  ]


type alias View =
  Signal.Address Action -> Model -> Html


main : Signal Html
main =
  app.html


app : StartApp.App Model
app =
  StartApp.start
    { init = ( load getStorage init, Effects.none )
    , view = view
    , update = \a m -> ( update a m, Effects.none )
    , inputs = inputs
    }


init : Model
init =
  let
    stats =
      BattleStats.init

    equip =
      Equipment.init

    map =
      Map.init

    battleContext =
      { stats = stats
      , equipment = equip
      , map = map
      }
  in
    { inventory = Inventory.init
    , battle = Battle.init battleContext
    , stats = stats
    , equipment = equip
    , buildings = Buildings.init
    , map = map
    , mana = Mana.init
    , activeMainTab = BattleTab
    , loadError = Nothing
    , shouldSave = True
    , shouldClearSave = False
    }


inputs : List (Signal Action)
inputs =
  [ fps 60 |> Signal.map Tick
  , Keys.pressed
      |> Signal.map KeyPress
  , didClearStorage
      |> Signal.map DidClearSave
  ]


view : View
view address model =
  let
    fwd =
      Signal.forwardTo address

    viewTab tab =
      button [ onClick address <| ChooseTab tab ] [ text <| .name <| tabData tab ]

    tabs =
      List.map viewTab allTabs

    saveErr =
      case model.loadError of
        Nothing ->
          []

        Just errorMessage ->
          [ text errorMessage ]

    inline extra =
      [ style ([ display InlineBlock, verticalAlign Top ] ++ extra) ]
  in
    div
      []
      ([ div
          (inline [])
          [ lazy Inventory.view model.inventory
          , lazy (Mana.view <| fwd ManaAction) model.mana
          , lazy2 (BattleStats.view <| fwd StatsAction) model.equipment model.stats
          ]
       , div
          (inline [ minWidth (Px 400) ])
          (tabs ++ [ (tabData model.activeMainTab).view address model ])
       ]
        ++ saveErr
        ++ [ button [ onClick address ClearSave ] [ text "DELETE SAVE" ] ]
      )


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Tick delta ->
      let
        dT =
          inSeconds delta

        wrapped =
          [ BattleAction << Battle.Tick
          , StatsAction << BattleStats.Tick
          , BuildingAction << Buildings.Tick
          , EquipAction << Equipment.Tick
          , ManaAction << Mana.Tick
          ]

        actions =
          List.map (\f -> update <| f dT) wrapped
      in
        List.foldl (\f m -> f m) model actions

    ChooseTab tab ->
      { model | activeMainTab = tab }

    BattleAction bAction ->
      let
        ( updatedBattle, output ) =
          Battle.update bAction model model.battle

        mapUpdate =
          case output.mapAction of
            Just action ->
              update (MapAction action)

            Nothing ->
              identity
      in
        { model
          | battle = updatedBattle
          , inventory = Inventory.applyRewards output.rewards model.inventory
        }
          |> mapUpdate

    StatsAction sAction ->
      let
        statUpdate =
          BattleStats.update sAction model.stats

        statSetter s m =
          { m | stats = s }
      in
        tryPurchase statSetter statUpdate model

    EquipAction eAction ->
      let
        ( updatedEquip, effects ) =
          Equipment.update eAction model.equipment

        equipSetter e m =
          { m | equipment = e }
      in
        trySpend effects equipSetter updatedEquip model

    BuildingAction action ->
      let
        ( updatedBuildings, effects ) =
          Buildings.update action model.buildings

        buildingSetter b m =
          { m | buildings = b }
      in
        { model
          | inventory = Inventory.applyFloatRewards effects.reward model.inventory
        }
          |> trySpend effects buildingSetter updatedBuildings

    MapAction action ->
      let
        ( updatedMap, didChange ) =
          Map.update action model.map

        battleUpdate =
          if didChange then
            update (BattleAction Battle.ChangedMap)
          else
            identity
      in
        { model | map = updatedMap }
          |> battleUpdate

    ManaAction action ->
      { model | mana = Mana.update action model.mana }

    KeyPress key ->
      let
        ( updatedBattle, _ ) =
          Battle.update (Battle.KeyPress key) model model.battle
      in
        { model | battle = updatedBattle }

    ClearSave ->
      { model
        | shouldClearSave = True
        , shouldSave = False
      }

    DidClearSave clear ->
      if clear then
        init
      else
        { model | shouldClearSave = False }


tabData : Tab -> { name : String, view : View }
tabData tab =
  case tab of
    BattleTab ->
      { name = "Battle"
      , view =
          \address model ->
            Battle.view (Signal.forwardTo address BattleAction) model model.battle
      }

    EquipmentTab ->
      { name = "Equipment"
      , view =
          \address model ->
            lazy (Equipment.view <| Signal.forwardTo address EquipAction) model.equipment
      }

    BuildingsTab ->
      { name = "Buildings"
      , view =
          \address model ->
            lazy (Buildings.view <| Signal.forwardTo address BuildingAction) model.buildings
      }

    MapTab ->
      { name = "Map"
      , view =
          \address model ->
            lazy (Map.view <| Signal.forwardTo address MapAction) model.map
      }


trySpend : { a | cost : List Currency.Bundle, manaCost : Int } -> (b -> Model -> Model) -> b -> Model -> Model
trySpend costs setter updated model =
  let
    hasCost =
      List.length costs.cost > 0

    hasManaCost =
      costs.manaCost > 0
  in
    if hasCost && hasManaCost then
      let
        canSpendMana =
          Mana.canSpend costs.manaCost model.mana

        canSpendCurrency =
          isOk <| Inventory.spendAll costs.cost model.inventory
      in
        if canSpendMana && canSpendCurrency then
          trySpendMana setter ( updated, costs.manaCost ) model
            |> tryPurchase setter ( updated, costs.cost )
        else
          model
    else if hasManaCost then
      trySpendMana setter ( updated, costs.manaCost ) model
    else
      tryPurchase setter ( updated, costs.cost ) model


tryPurchase : (a -> Model -> Model) -> ( a, List Currency.Bundle ) -> Model -> Model
tryPurchase setter ( updated, cost ) model =
  let
    result =
      Inventory.spendAll cost model.inventory
  in
    case result of
      Ok updatedInventory ->
        setter updated { model | inventory = updatedInventory }

      Err _ ->
        model


trySpendMana : (a -> Model -> Model) -> ( a, Int ) -> Model -> Model
trySpendMana setter ( updated, cost ) model =
  let
    mana =
      model.mana

    success =
      Mana.canSpend cost mana
  in
    if success then
      setter updated { model | mana = Mana.trySpend cost mana mana }
    else
      model


serializer : Serialize.Serializer Model
serializer =
  Serialize.object6
    init
    ( "inventory", Focus.create .inventory (\f m -> { m | inventory = f m.inventory }), Inventory.serializer )
    ( "stats", Focus.create .stats (\f m -> { m | stats = f m.stats }), BattleStats.serializer )
    ( "map", Focus.create .map (\f m -> { m | map = f m.map }), Map.serializer )
    ( "equipment", Focus.create .equipment (\f m -> { m | equipment = f m.equipment }), Equipment.serializer )
    ( "buildings", Focus.create .buildings (\f m -> { m | buildings = f m.buildings }), Buildings.serializer )
    ( "mana", Focus.create .mana (\f m -> { m | mana = f m.mana }), Mana.serializer )


load : Maybe String -> Model -> Model
load storage model =
  case storage of
    Nothing ->
      -- no save data
      model

    Just save ->
      case Decode.decodeString serializer.decoder save of
        Err err ->
          -- save could not be read
          { model
            | loadError = Just err
            , shouldSave = False
          }

        Ok loaded ->
          -- save was loaded successfully
          loaded


port getStorage : Maybe String
port didClearStorage : Signal Bool
port setStorage : Signal String
port setStorage =
  let
    encode model =
      if model.shouldSave then
        Encode.encode 0 (Serialize.encode serializer model)
      else
        ""
  in
    Signal.map encode app.model
      |> Signal.sampleOn (Time.every <| 1 * Time.second)


port clearStorage : Signal Bool
port clearStorage =
  Signal.map .shouldClearSave app.model
