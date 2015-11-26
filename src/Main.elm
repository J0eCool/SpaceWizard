import Effects
import Html exposing (span, div, button, text, h3, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Format
import Signal
import StartApp
import String
import Time exposing (inSeconds, fps)


main : Signal Html.Html
main =
    app.html

app : StartApp.App Model
app =
    StartApp.start { init = init, view = view, update = update, inputs = inputs }

type alias Model =
    { score : Int
    , scorePerSecond : Float
    , partialScore : Float
    , scorePerClick : Int
    }

init : (Model, Effects.Effects Action)
init =
    ( { score = 0, scorePerSecond = 0, partialScore = 0, scorePerClick = 1 }
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
        , viewShop address model
        ]

viewScore : Signal.Address Action -> Model -> Html.Html
viewScore address model =
    div []
        [ h3 [] [text "Score"]
        , div [] [text <| toString model.score ++ " (+" ++ toString model.scorePerSecond ++ "/s)"]
        , viewScoreClickButton address model
        ]

viewScoreClickButton : Signal.Address Action -> Model -> Html.Html
viewScoreClickButton address model =
    let clickButtonText = "Click (+" ++ toString model.scorePerClick ++ ")"
    in button [onClick address ScoreClick] [text clickButtonText]

viewShop : Signal.Address Action -> Model -> Html.Html
viewShop address model =
    div []
        [ h3 [] [text "Shop"]
        , ul []
            [ li []
                [ span [] [text "Upgrade click power:"]
                , button [onClick address UpgradeClick] [text <| toString <| upgradeClickCost model]
                ]
            , li []
                [ span [] [text "Upgrade score per second:"]
                , button [onClick address UpgradePerSecond] [text <| toString <| upgradePerSecondCost model]
                ]
            ]
        ]

upgradeClickCost : Model -> Int
upgradeClickCost model = floor <| (toFloat model.scorePerClick) ^ 1.5 * 10

upgradePerSecondCost : Model -> Int
upgradePerSecondCost model = floor <| (model.scorePerSecond + 1) ^ 1.25 * 5


type Action
    = ScoreClick
    | UpgradeClick
    | UpgradePerSecond
    | TimeDelta Float


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
            in updateScoreTime dT model

updateScoreClick : Model -> Model
updateScoreClick model =
    { model | score = model.score + model.scorePerClick }

updateUpgradeClick : Model -> Model
updateUpgradeClick model =
    let cost = upgradeClickCost model
    in
        if model.score < cost
        then model
        else { model | score = model.score - cost, scorePerClick = model.scorePerClick + 1 }

updateUpgradePerSecond : Model -> Model
updateUpgradePerSecond model =
    let cost = upgradePerSecondCost model
    in
        if model.score < cost
        then model
        else { model
            | score = model.score - cost
            , scorePerSecond = model.scorePerSecond + 0.25
            }

updateScoreTime : Float -> Model -> Model
updateScoreTime dT model =
    let partial = model.partialScore + model.scorePerSecond * dT
        delta = floor <| partial
    in { model
        | partialScore = partial - toFloat delta
        , score = model.score + delta
        }
