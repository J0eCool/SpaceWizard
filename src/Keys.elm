module Keys where

import Char exposing (KeyCode)
import Keyboard

type Key
    = KeyChar Char
    | KeyArrow Arrow

type Arrow
    = Left
    | Right
    | Down
    | Up
    | None

type alias ArrowInput =
    { x : Int
    , y : Int
    }

pressed : Signal Key
pressed =
    let presses =
            Keyboard.presses |> Signal.map keyChar
        baseArrow =
            ({x = 0, y = 0}, None)
        arrows =
            Keyboard.arrows
                |> Signal.foldp updateArrow baseArrow
                |> Signal.map (KeyArrow << snd)
    in Signal.merge presses arrows

keyChar : KeyCode -> Key
keyChar code =
    KeyChar <| Char.fromCode code

updateArrow : ArrowInput -> (ArrowInput, Arrow) -> (ArrowInput, Arrow)
updateArrow new (old, _) =
    ( new
    , if old.x == 0 && new.x /= 0 then
        if new.x > 0 then
            Right
        else
            Left
    else if old.y == 0 && new.y /= 0 then
        if new.y > 0 then
            Up
        else
            Down
    else
        None
    )
