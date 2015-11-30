import Effects
import Html exposing (span, div, button, text, h3, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Signal
import StartApp
import String
import Time exposing (inSeconds, fps)

import Battle
import Currency
import Format
import Inventory

type alias Model =
    { inventory : Inventory.Model
    , battle : Battle.Model
    }

type Action
    = TryPurchase Currency.Bundle Action
    | BattleAction Battle.Action
    | Tick Float

main : Signal Html.Html
main =
    app.html

app : StartApp.App Model
app =
    StartApp.start
        { init = init
        , view = view
        , update = update
        , inputs = inputs
        }

init : (Model, Effects.Effects Action)
init =
    ( { inventory = Inventory.init
      , battle = Battle.init
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
            TryPurchase cost <| actionType action
        shopAddress =
            Signal.forwardTo address (forward BattleAction)
    in div []
        [ Inventory.view model.inventory
        , Battle.view shopAddress model.battle
        ]

update : Action -> Model -> (Model, Effects.Effects a)
update action model =
    ( case action of
        TryPurchase cost successfulAction ->
            let result =
                Inventory.spend cost model.inventory
            in case result of
                Ok inventory' ->
                    { model | inventory = inventory' }
                        |> update successfulAction
                        |> fst
                Err _ ->
                    model
        Tick delta ->
            let dT = inSeconds delta
                updates = [BattleAction << Battle.Tick]
                (model', _) = update (BattleAction <| Battle.Tick dT) model
            in model'
        BattleAction bAction ->
            let (battle', battleRewards) = Battle.update bAction model.battle
            in { model
                | battle = battle'
                , inventory = Inventory.update battleRewards model.inventory
                }
    , Effects.none)
