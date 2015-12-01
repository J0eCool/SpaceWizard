import Effects
import Html exposing (span, div, button, text, h3, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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
    = TryPurchase (Currency.Bundle, Action)
    | BattleAction Battle.Action
    | StatsAction BattleStats.Action
    | Tick Float

main : Signal Html.Html
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
    ]

view : Signal.Address Action -> Model -> Html.Html
view address model =
    let forward actionType (cost, action) =
            TryPurchase (cost, actionType action)
        shopAddress =
            Signal.forwardTo address (forward StatsAction)
    in div []
        [ Inventory.view model.inventory
        , Battle.view model.stats model.battle
        , BattleStats.view shopAddress model.stats
        ]

update : Action -> Model -> Model
update action model =
    case action of
        TryPurchase (cost, successfulAction) ->
            let result =
                Inventory.spend cost model.inventory
            in case result of
                Ok inventory' ->
                    { model | inventory = inventory' }
                        |> update successfulAction
                Err _ ->
                    model
        Tick delta ->
            let dT = inSeconds delta
            in update (BattleAction <| Battle.Tick dT) model
        BattleAction bAction ->
            let (battle', battleRewards) = Battle.update bAction model.stats model.battle
            in { model
                | battle = battle'
                , inventory = Inventory.update battleRewards model.inventory
                }
        StatsAction sAction ->
            { model | stats = BattleStats.update sAction model.stats }
