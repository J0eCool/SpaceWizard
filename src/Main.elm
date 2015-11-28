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
    , scorePerSecond : Float
    , partialScore : Float
    , scorePerClick : Int
    , battle : Battle.Model
    }

type Action
    = ScoreClick
    | UpgradeClick
    | UpgradePerSecond
    | TryPurchase Currency.Bundle Action
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
      , scorePerSecond = 0
      , partialScore = 0
      , scorePerClick = 1
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
    div []
        [ Inventory.view model.inventory
        , Battle.view model.battle
        , viewShop address model
        ]

viewScore : Signal.Address Action -> Model -> Html.Html
viewScore address model =
    div []
        [ h3 [] [text "Inventory"]
        , div [] [text
            <| Format.int (Inventory.get Currency.Gold model.inventory)
            ++ " (+"
            ++ Format.float model.scorePerSecond
            ++ "/s)"
            ]
        , viewScoreClickButton address model
        ]

viewScoreClickButton : Signal.Address Action -> Model -> Html.Html
viewScoreClickButton address model =
    let clickButtonText = "Click (+" ++ Format.int model.scorePerClick ++ ")"
    in button [onClick address ScoreClick] [text clickButtonText]

viewShop : Signal.Address Action -> Model -> Html.Html
viewShop address model =
    div []
        [ h3 [] [text "Shop"]
        , ul []
            [ li []
                [ span [] [text "Upgrade click power:"]
                , button
                    [onClick address
                        <| TryPurchase (upgradeClickCost model) UpgradeClick]
                    [text <| Format.currency <| upgradeClickCost model]
                ]
            , li []
                [ span [] [text "Upgrade score per second:"]
                , button
                    [onClick address
                        <| TryPurchase (upgradePerSecondCost model) UpgradePerSecond]
                    [text <| Format.currency <| upgradePerSecondCost model]
                ]
            ]
        ]

upgradeClickCost : Model -> Currency.Bundle
upgradeClickCost model =
    let cost = floor <| (toFloat model.scorePerClick) ^ 1.5 * 10
    in (Currency.Gold, cost)

upgradePerSecondCost : Model -> Currency.Bundle
upgradePerSecondCost model =
    let cost = floor <| (model.scorePerSecond + 1) ^ 1.25 * 5
    in (Currency.Gold, cost)

update : Action -> Model -> (Model, Effects.Effects a)
update action model =
    ( case action of
        ScoreClick -> updateScoreClick model
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
        UpgradeClick -> updateUpgradeClick model
        UpgradePerSecond -> updateUpgradePerSecond model
        Tick delta ->
            let dT = inSeconds delta
                (battle', battleRewards) = Battle.update (Battle.Tick dT) model.battle
            in { model
                | battle = battle'
                , inventory = Inventory.update battleRewards model.inventory
                }
                |> updateScoreTime dT
    , Effects.none)

updateScoreClick : Model -> Model
updateScoreClick model =
    { model
        | inventory = Inventory.gain (Currency.Gold, model.scorePerClick) model.inventory
        }

updateUpgradeClick : Model -> Model
updateUpgradeClick model =
    { model | scorePerClick = model.scorePerClick + 1 }

updateUpgradePerSecond : Model -> Model
updateUpgradePerSecond model =
    { model | scorePerSecond = model.scorePerSecond + 0.25 }

updateScoreTime : Float -> Model -> Model
updateScoreTime dT model =
    let partial = model.partialScore + model.scorePerSecond * dT
        delta = floor <| partial
    in { model
        | partialScore = partial - toFloat delta
        , inventory = Inventory.gain (Currency.Gold, delta) model.inventory
        }
