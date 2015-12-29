import Char exposing (KeyCode)
import Effects
import Html exposing (Html, span, div, button, text, h3, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Keyboard
import Signal
import StartApp
import String
import Time exposing (inSeconds, fps)

import Battle
import BattleStats
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
  }

type Action
  = Tick Float
  | BattleAction Battle.Action
  | StatsAction BattleStats.Action
  | EquipAction Equipment.Action
  | KeyPress Keys.Key

main : Signal Html
main =
  app.html

app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , view = view
    , update = \a m -> (update a m, Effects.none)
    , inputs = inputs
    }

init : (Model, Effects.Effects Action)
init =
  let
    stats = BattleStats.init
    equip = Equipment.init
    battleContext =
      { stats = stats
      , equip = equip
      }
  in
    ( { inventory = Inventory.init
      , battle = Battle.init battleContext
      , stats = stats
      , equipment = equip
      }
    , Effects.none
    )

inputs : List (Signal Action)
inputs =
  [ fps 60 |> Signal.map Tick
  , Keys.pressed |> Signal.map KeyPress
  ]

view : Signal.Address Action -> Model -> Html
view address model =
  let
    fwd =
      Signal.forwardTo address
  in div []
    [ Battle.view (fwd BattleAction) (battleContext model) model.battle
    , lazy Inventory.view model.inventory
    , lazy2 (BattleStats.view <| fwd StatsAction) model.equipment model.stats
    , lazy (Equipment.view <| fwd EquipAction) model.equipment
    ]

update : Action -> Model -> Model
update action model =
  case action of
    Tick delta ->
      let
        dT = inSeconds delta
        wrapped =
          [ BattleAction << Battle.Tick
          , StatsAction << BattleStats.Tick
          ]
        actions =
          List.map (\f -> update <| f dT) wrapped
      in List.foldl (\f m -> f m) model actions
    BattleAction bAction ->
      let
        (battle', battleRewards) =
          Battle.update bAction (battleContext model) model.battle
      in
        { model
        | battle = battle'
        , inventory = Inventory.update battleRewards model.inventory
        }
    StatsAction sAction ->
      let (stats', statCost) =
        BattleStats.update sAction model.stats
      in tryPurchase statCost model { model | stats = stats' }
    EquipAction eAction ->
      let
        updatedEquipment =
          Equipment.update eAction model.equipment
      in { model | equipment = updatedEquipment }
    KeyPress key ->
      let
        battle' =
          Battle.update
            (Battle.KeyPress key)
            (battleContext model)
            model.battle
            |> fst
      in { model | battle = battle' }

battleContext : Model -> Battle.Context
battleContext model =
  { stats = model.stats
  , equip = model.equipment
  }

tryPurchase cost model successfulModel =
  let result =
    Inventory.spendAll cost successfulModel.inventory
  in case result of
    Ok inventory' ->
      { successfulModel
      | inventory = inventory'
      }
    Err _ ->
      model
