module Battle where

import Color
import Html exposing (div, h3, text, ul, li)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import BattleStats exposing (attackDamage, attackSpeed, goldBonusMultiplier)
import Currency
import Format
import Style exposing (..)

type alias Model =
    { health : Int
    , maxHealth : Int
    , attackTimer : Float
    , gold : Int
    , experience : Int
    }

type Action
    = Tick Float

init : Model
init =
    { health = 50
    , maxHealth = 50
    , attackTimer = 0
    , gold = 8
    , experience = 1
    }

update : Action -> BattleStats.Model -> Model -> (Model, List Currency.Bundle)
update action stats model =
    case action of
        Tick dT ->
            updateTick dT stats model

updateTick : Float -> BattleStats.Model -> Model -> (Model, List Currency.Bundle)
updateTick dT stats model =
    let updatedTimer =
            model.attackTimer + dT
        timeToAttack =
            1 / attackSpeed stats
        maxNumAttacks = 3
        numAttacks =
            updatedTimer / timeToAttack
                |> floor
                |> min maxNumAttacks
        updatedHealth = 
            model.health - numAttacks * attackDamage stats
        didDie =
            updatedHealth <= 0
    in ( { model
            | attackTimer =
                if numAttacks >= maxNumAttacks then
                    0
                else
                    updatedTimer - toFloat numAttacks * timeToAttack
            , health =
                if didDie then
                    model.maxHealth
                else
                    updatedHealth
            }
        ,   if didDie then
                reward stats model
            else
                []
        )

view : BattleStats.Model -> Model -> Html.Html
view stats model =
    div []
        [ h3 [] [text "Battle"]
        , div [] [text <| "Health: " ++ Format.int model.health]
        , div 
            [ style
                [ width <| px 300
                , height <| px 50
                , backgroundColor Color.darkRed
                ]
            ]
            [ div
                [ style
                    [ width <| pct <| 100 * model.health // model.maxHealth
                    , height <| pct 100
                    , backgroundColor Color.red
                    ]
                ]
                []
            ]
        , div [] [text "Reward: "]
        , ul []
            <|  let currency = reward stats model
                    item c = li [] [text <| Format.currency c]
                in List.map item currency
        ]

reward : BattleStats.Model -> Model -> List Currency.Bundle
reward stats model =
    [   ( Currency.Gold
        , round <| toFloat model.gold * goldBonusMultiplier stats
        )
    ,   ( Currency.Experience
        , model.experience
        )
    ]
