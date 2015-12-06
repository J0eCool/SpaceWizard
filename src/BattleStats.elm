module BattleStats where

import Html exposing (Html, div, h3, text, span, button, ul, li)
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

type Cost
    = TotalCost Float Float Float

type alias Stat =
    { level : Int
    , growth : Growth
    , cost : Cost
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
    }

baseStatCost = TotalCost 0.5 3 (-4.5)

update : Action -> Model -> Model
update action model =
    case action of
        UpgradeStrength ->
            { model | strength = levelUp model.strength }
        UpgradeSpeed ->
            { model | speed = levelUp model.speed }
        UpgradeLuck ->
            { model | luck = levelUp model.luck }

view : Signal.Address (Currency.Bundle, Action) -> Model -> Html
view address model =
    div []
        [ h3 [] [text "Stats"]
        , viewBaseStats address model
        , viewDerivedStats model
        ]

viewBaseStats : Signal.Address (Currency.Bundle, Action) -> Model -> Html
viewBaseStats address model =
    let viewStat (title, field, action) =
            let stat = field model
                curCost = cost 1 stat
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
                , ("Gold Bonus %", f << goldBonus)
                ]
    in ul [] items

levelUp : Stat -> Stat
levelUp stat =
    { stat | level = stat.level + 1 }

value : Stat -> Float
value stat =
    growthValue stat.level stat.growth

cost : Float -> Stat -> Currency.Bundle
cost delta stat =
    let cur = totalCostValue (toFloat stat.level) stat.cost
        next = totalCostValue (toFloat stat.level + delta) stat.cost
    in  ( Currency.Experience
        , floor <| next - cur
        )

totalCostValue : Float -> Cost -> Float
totalCostValue level (TotalCost a b c) =
    level * (c + level * (b + level * a)) -- ax^3 + bx^2 + cx

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
