import Char exposing (KeyCode)
import Effects
import Html exposing (Html, span, div, button, text, h3, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Keyboard
import Signal
import StartApp
import String
import Time exposing (inSeconds, fps)

import Battle
import BattleStats
import Currency
import Format
import Inventory

type alias Model =
    { inventory : Inventory.Model
    , battle : Battle.Model
    , stats : BattleStats.Model
    }

type Action
    = Tick Float
    | BattleAction Battle.Action
    | StatsAction BattleStats.Action
    | KeyPress KeyCode

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
    ( { inventory = Inventory.init
      , battle = Battle.init
      , stats = BattleStats.init
      }
    , Effects.none
    )

inputs : List (Signal Action)
inputs =
    [ fps 60 |> Signal.map Tick
    , Keyboard.presses |> Signal.map KeyPress
    ]

view : Signal.Address Action -> Model -> Html
view address model =
    let battleAddress =
            Signal.forwardTo address BattleAction
        shopAddress =
            Signal.forwardTo address StatsAction
    in div []
        [ Inventory.view model.inventory
        , Battle.view battleAddress model.stats model.battle
        , BattleStats.view shopAddress model.stats
        ]

update : Action -> Model -> Model
update action model =
    case action of
        Tick delta ->
            let dT = inSeconds delta
                wrapped =
                    [ BattleAction << Battle.Tick
                    , StatsAction << BattleStats.Tick
                    ]
                actions =
                    List.map (\f -> update <| f dT) wrapped
            in List.foldl (\f m -> f m) model actions
        BattleAction bAction ->
            let (battle', battleRewards) =
                Battle.update bAction model.stats model.battle
            in { model
                | battle = battle'
                , inventory = Inventory.update battleRewards model.inventory
                }
        StatsAction sAction ->
            let (stats', statCost) =
                BattleStats.update sAction model.stats
            in tryPurchase statCost model { model | stats = stats' }
        KeyPress key ->
            let battle' =
                fst <| Battle.update (Battle.KeyPress key) model.stats model.battle
            in { model | battle = battle' }

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
