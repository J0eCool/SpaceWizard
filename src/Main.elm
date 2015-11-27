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

type alias Model =
    { inventory : Currency.Inventory
    , scorePerSecond : Float
    , partialScore : Float
    , scorePerClick : Int
    , battle : Battle.Model
    }

type Action
    = ScoreClick
    | UpgradeClick
    | UpgradePerSecond
    | TimeDelta Float

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
    ( { inventory = Currency.emptyInventory
      , scorePerSecond = 0
      , partialScore = 0
      , scorePerClick = 1
      , battle = Battle.init
      }
    , Effects.none
    )

inputs : List (Signal Action)
inputs =
    [ fps 60 |> Signal.map TimeDelta
    ]

view : Signal.Address Action -> Model -> Html.Html
view address model =
    div []
        [ viewScore address model
        , Battle.view model.battle
        , viewShop address model
        ]

viewScore : Signal.Address Action -> Model -> Html.Html
viewScore address model =
    div []
        [ h3 [] [text "Inventory"]
        , div [] [text
            <| Format.int (Currency.get Currency.Gold model.inventory)
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
                , button [onClick address UpgradeClick] [text <| Format.int <| upgradeClickCost model]
                ]
            , li []
                [ span [] [text "Upgrade score per second:"]
                , button [onClick address UpgradePerSecond] [text <| Format.int <| upgradePerSecondCost model]
                ]
            ]
        ]

upgradeClickCost : Model -> Int
upgradeClickCost model = floor <| (toFloat model.scorePerClick) ^ 1.5 * 10

upgradePerSecondCost : Model -> Int
upgradePerSecondCost model = floor <| (model.scorePerSecond + 1) ^ 1.25 * 5

update : Action -> Model -> (Model, Effects.Effects a)
update action model = (updateModel action model, Effects.none)

updateModel : Action -> Model -> Model
updateModel action model =
    case action of
        ScoreClick -> updateScoreClick model
        UpgradeClick -> updateUpgradeClick model
        UpgradePerSecond -> updateUpgradePerSecond model
        TimeDelta delta ->
            let dT = inSeconds delta
                (battle', battleRewards) = Battle.update dT model.battle
            in { model
                | battle = battle'
                }
                |> updateScoreTime dT
                |> applyRewards battleRewards

applyRewards : List Currency.Reward -> Model -> Model
applyRewards rewards model =
    { model
        | inventory = Currency.applyRewards rewards model.inventory
        }

updateScoreClick : Model -> Model
updateScoreClick model =
    { model
        | inventory = Currency.gain Currency.Gold model.scorePerClick model.inventory
        }

updateUpgradeClick : Model -> Model
updateUpgradeClick model =
    let cost = upgradeClickCost model
        (inventory', didBuy) =
            Currency.spend Currency.Gold cost model.inventory
    in { model
        | inventory = inventory'
        , scorePerClick =
            if didBuy then
                model.scorePerClick + 1
            else
                model.scorePerClick
        }

updateUpgradePerSecond : Model -> Model
updateUpgradePerSecond model =
    let cost = upgradePerSecondCost model
        (inventory', didBuy) =
            Currency.spend Currency.Gold cost model.inventory
    in { model
        | inventory = inventory'
        , scorePerSecond =
            if didBuy then
                model.scorePerSecond + 0.25
            else
                model.scorePerSecond
        }

updateScoreTime : Float -> Model -> Model
updateScoreTime dT model =
    let partial = model.partialScore + model.scorePerSecond * dT
        delta = floor <| partial
    in { model
        | partialScore = partial - toFloat delta
        , inventory = Currency.gain Currency.Gold delta model.inventory
        }
