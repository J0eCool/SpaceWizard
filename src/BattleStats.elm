module BattleStats where

import Color
import Html exposing (Html, div, h3, text, span, button, ul, li)
import Html.Events exposing (onMouseDown, onMouseUp, onMouseEnter, onMouseLeave)

import Currency
import Format
import Style exposing (..)
import Widgets.ProgressBar as ProgressBar

type alias Model =
    { strength : Stat
    , speed : Stat
    , luck : Stat
    , weapon : Stat
    , heldAction : Maybe TimedAction
    , hoveredUpgrade : Maybe TimedAction
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
    = SetHeld TimedAction
    | SetHover TimedAction
    | Release
    | Tick Float

type TimedAction
    = UpgradeStrength
    | UpgradeSpeed
    | UpgradeLuck
    | UpgradeWeapon

init : Model
init = 
    { strength =
        { level = 1
        , growth = LinearGrowth 1 0.1
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
    , weapon =
        { level = 1
        , growth = LinearGrowth 20 5
        , cost =
            ( Currency.Gold
            , TotalCost 2 2 1
            )
        }
    , heldAction = Nothing
    , hoveredUpgrade = Nothing
    }

baseStatCost : CostBundle
baseStatCost =
    ( Currency.Experience
    , TotalCost 0.5 3 (-4.5)
    )

update : Action -> Model -> (Model, List Currency.Bundle)
update action model =
    let no m = (m, [])
    in case action of
        SetHeld action ->
            no { model | heldAction = Just action }
        SetHover action ->
            no { model | hoveredUpgrade = Just action }
        Release ->
            { model
            | heldAction = Nothing
            , hoveredUpgrade = Nothing
            }
            |> no
        Tick dT ->
            case model.heldAction of
                Just act ->
                    upgradeBy dT act model
                Nothing ->
                    no model

upgradeBy : Float -> TimedAction -> Model -> (Model, List Currency.Bundle)
upgradeBy amount action model =
    let stat =
            case action of
                UpgradeStrength -> model.strength
                UpgradeSpeed -> model.speed
                UpgradeLuck -> model.luck
                UpgradeWeapon -> model.weapon
        spent =
            cost amount stat
        updatedStat =
            levelUp amount stat
        updatedModel =
            case action of
                UpgradeStrength ->
                    { model | strength = updatedStat }
                UpgradeSpeed ->
                    { model | speed = updatedStat }
                UpgradeLuck ->
                    { model | luck = updatedStat }
                UpgradeWeapon ->
                    { model | weapon = updatedStat }
    in (updatedModel, [spent])

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
                    [ onMouseDown address <| SetHeld action
                    , onMouseEnter address <| SetHover action
                    , onMouseUp address <| Release
                    , onMouseLeave address <| Release
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
                , ("Weapon", .weapon, UpgradeWeapon)
                ]
    in ul [] items

viewDerivedStats : Model -> Html
viewDerivedStats model =
    let upgradedModel =
            case model.hoveredUpgrade of
                Just action ->
                    fst <| upgradeBy 1 action model
                Nothing ->
                    model
        viewStat (title, format, stat) =
            let curStat =
                    stat model
                diff =
                    stat upgradedModel - curStat
                diffSpan =
                    if abs diff > 0.01 then
                        [ span
                            [style
                                [ color <| Color.rgb 32 200 64 
                                , fontWeight Bold
                                ]
                            ]
                            [text <| " (+" ++ format diff ++ ")"] ]
                    else
                        []
            in li []
                ([ span [] [text <| title ++ ": " ++ format curStat] ]
                ++ diffSpan
                )
        i = Format.int << round
        f = Format.float
        items =
            List.map viewStat
                [ ("Attack Damage", i, toFloat << attackDamage)
                , ("Attack Speed", f, attackSpeed)
                , ("DPS", f, \m -> attackSpeed m * toFloat (attackDamage m))
                , ("Weapon base damage", i, toFloat << weaponDamage)
                , ("Gold Bonus %", f, goldBonus)
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
    let (type', cost) = stat.cost
        cur = totalCostValue stat.level cost
        next = totalCostValue (stat.level + delta) cost
    in  ( type'
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

weaponDamage : Model -> Int
weaponDamage model =
    round <| value model.weapon

attackDamage : Model -> Int
attackDamage model =
    let wep = toFloat <| weaponDamage model
        str = value model.strength
    in round <| wep * str

attackSpeed : Model -> Float
attackSpeed model =
    value model.speed

goldBonus : Model -> Float
goldBonus model =
    value model.luck

goldBonusMultiplier : Model -> Float
goldBonusMultiplier model =
    1 + goldBonus model / 100
