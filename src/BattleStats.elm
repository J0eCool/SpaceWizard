module BattleStats where

import Color
import Html exposing (Html, div, h3, text, span, button, ul, li)
import Html.Events exposing (onMouseDown, onMouseUp, onMouseLeave)

import Currency
import Format
import Style exposing (..)
import Widgets.ProgressBar as ProgressBar

type alias Model =
    { strength : Stat
    , speed : Stat
    , luck : Stat
    , heldAction : Maybe TimedAction
    }

type Growth
    = LinearGrowth Float Float
    | PowerGrowth Float Float Float

type Cost
    = TotalCost Float Float Float

type alias CostBundle =
    (Currency.Type, Cost)

type alias Stat =
    { level : Float
    , growth : Growth
    , cost : CostBundle
    }

type Action
    = SetHeld (Maybe TimedAction)
    | Tick Float

type TimedAction
    = UpgradeStrength
    | UpgradeSpeed
    | UpgradeLuck

init : Model
init = 
    { strength =
        { level = 1
        , growth = LinearGrowth 20 2
        , cost = baseStatCost
        }
    , speed =
        { level = 1
        , growth = LinearGrowth 1.2 0.1
        , cost = baseStatCost
        }
    , luck =
        { level = 1
        , growth = LinearGrowth 0 15
        , cost = baseStatCost
        }
    , heldAction = Nothing
    }

baseStatCost : CostBundle
baseStatCost =
    ( Currency.Experience
    , TotalCost 0.5 3 (-4.5)
    )

update : Action -> Model -> (Model, List Currency.Bundle)
update action model =
    case action of
        SetHeld action ->
            ({ model | heldAction = action }, [])
        Tick dT ->
            case model.heldAction of
                Just act ->
                    updateTick dT act model
                Nothing ->
                    (model, [])

updateTick : Float -> TimedAction -> Model -> (Model, List Currency.Bundle)
updateTick dT action model =
    case action of
        UpgradeStrength ->
            ( { model | strength = levelUp dT model.strength }
            , [ cost dT model.strength ]
            )
        UpgradeSpeed ->
            ( { model | speed = levelUp dT model.speed }
            , [ cost dT model.speed ]
            )
        UpgradeLuck ->
            ( { model | luck = levelUp dT model.luck }
            , [ cost dT model.luck ]
            )

view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ h3 [] [text "Stats"]
        , viewBaseStats address model
        , viewDerivedStats model
        ]

viewBaseStats : Signal.Address Action -> Model -> Html
viewBaseStats address model =
    let viewStat (title, field, action) =
            let stat = field model
                curCost = cost 1 stat
            in li []
                [ span 
                    [ style
                        [ display InlineBlock
                        , width <| Px 120
                        ]

                    ]
                    [ text
                        <| title ++ ": "
                        ++ Format.float stat.level
                    ]
                , ProgressBar.viewInline
                    { width = 200
                    , height = 10
                    , color = Color.rgb 64 255 200
                    , background = Color.rgb 32 128 92
                    , curAmount = stat.level - toFloat (floor stat.level)
                    , maxAmount = 1
                    }
                , button
                    [ onMouseDown address (SetHeld <| Just action)
                    , onMouseUp address <| SetHeld Nothing
                    , onMouseLeave address <| SetHeld Nothing
                    ]
                    [ text
                        <| "+1 ("
                        ++ Format.currency curCost
                        ++ ")"
                    ]
                ]
        items =
            List.map viewStat
                [ ("Strength", .strength, UpgradeStrength)
                , ("Speed", .speed, UpgradeSpeed)
                , ("Luck", .luck, UpgradeLuck)
                ]
    in ul [] items

viewDerivedStats : Model -> Html
viewDerivedStats model =
    let viewStat (title, stat) =
            li []
                [ span [] [text <| title ++ ": " ++ stat model]
                ]
        i = Format.int
        f = Format.float
        items =
            List.map viewStat
                [ ("Attack Damage", i << attackDamage)
                , ("Attack Speed", f << attackSpeed)
                , ("DPS", \m -> f <| attackSpeed m * toFloat (attackDamage m))
                , ("Gold Bonus %", f << goldBonus)
                ]
    in ul [] items

levelUp : Float -> Stat -> Stat
levelUp delta stat =
    { stat | level = stat.level + delta }

value : Stat -> Float
value stat =
    growthValue stat.level stat.growth

cost : Float -> Stat -> Currency.Bundle
cost delta stat =
    let cur = totalCostValue stat.level stat.cost
        next = totalCostValue (stat.level + delta) stat.cost
    in  ( Currency.Experience
        , next - cur
        )

totalCostValue : Float -> Cost -> Int
totalCostValue level (TotalCost a b c) =
    floor <| level * (c + level * (b + level * a)) -- ax^3 + bx^2 + cx

growthValue : Float -> Growth -> Float
growthValue level growth =
    case growth of
        LinearGrowth base slope ->
            slope * (level - 1) + base
        PowerGrowth lin powFac pow ->
            lin * level + powFac * (level - 1) ^ pow

attackDamage : Model -> Int
attackDamage model =
    round <| value model.strength

attackSpeed : Model -> Float
attackSpeed model =
    value model.speed

goldBonus : Model -> Float
goldBonus model =
    value model.luck

goldBonusMultiplier : Model -> Float
goldBonusMultiplier model =
    1 + goldBonus model / 100
