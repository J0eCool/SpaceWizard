module BattleStats where

import Html exposing (div, h3, text, span, button, ul, li)
import Html.Events exposing (onClick)

import Currency
import Format

type alias Model =
    { strength : Stat
    , speed : Stat
    , luck : Stat
    }

type Growth
    = LinearGrowth Float Float
    | PowerGrowth Float Float Float

type alias Stat =
    { level : Int
    , growth : Growth
    , costGrowth : Growth
    }

type Action
    = UpgradeStrength
    | UpgradeSpeed
    | UpgradeLuck

init : Model
init = 
    { strength =
        { level = 1
        , growth = LinearGrowth 10 1
        , costGrowth = PowerGrowth 4 1 2
        }
    , speed =
        { level = 1
        , growth = LinearGrowth 1.2 0.1
        , costGrowth = PowerGrowth 7 5 2
        }
    , luck =
        { level = 1
        , growth = LinearGrowth 0 15
        , costGrowth = PowerGrowth 50 10 2
        }
    }

update : Action -> Model -> Model
update action model =
    case action of
        UpgradeStrength ->
            { model | strength = levelUp model.strength }
        UpgradeSpeed ->
            { model | speed = levelUp model.speed }
        UpgradeLuck ->
            { model | luck = levelUp model.luck }

view : Signal.Address (Currency.Bundle, Action) -> Model -> Html.Html
view address model =
    div []
        [ h3 [] [text "Stats"]
        , viewBaseStats address model
        , viewDerivedStats model
        ]

viewBaseStats : Signal.Address (Currency.Bundle, Action) -> Model -> Html.Html
viewBaseStats address model =
    let viewStat (title, field, action) =
            let stat = field model
                curCost = cost stat
            in li []
                [ span []
                    [ text
                        <| title ++ ": "
                        ++ Format.int stat.level
                    ]
                , button
                    [onClick address (curCost, action)]
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

viewDerivedStats : Model -> Html.Html
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
                , ("Gold Bonus %", f << goldBonus)
                ]
    in ul [] items

levelUp : Stat -> Stat
levelUp stat =
    { stat | level = stat.level + 1 }

value : Stat -> Float
value stat =
    growthValue stat.level stat.growth

cost : Stat -> Currency.Bundle
cost stat =
    ( Currency.Gold
    , floor <| growthValue stat.level stat.costGrowth
    )

growthValue : Int -> Growth -> Float
growthValue level growth =
    let lv = toFloat level
    in case growth of
        LinearGrowth base slope ->
            slope * (lv - 1) + base
        PowerGrowth lin powFac pow ->
            lin * lv + powFac * (lv - 1) ^ pow

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
