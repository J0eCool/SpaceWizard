module Operators (..) where


(./) : Int -> Int -> Float
(./) a b =
    toFloat a / toFloat b


(?>) : Maybe a -> (a -> Maybe b) -> Maybe b
(?>) =
    Maybe.andThen
