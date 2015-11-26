module Format (int, float) where

import String

int : Int -> String
int n =
    toString n
        |> String.toList
        |> List.reverse
        |> intersperseBy 3 ','
        |> List.reverse
        |> String.fromList

intersperseBy : Int -> a -> List a -> List a
intersperseBy count item list =
    let aux curCount curList =
        case (curCount, curList) of
            (_, []) ->
                []
            (0, xs) ->
                item :: aux count xs
            (c, x :: xs) ->
                x :: aux (c - 1) xs
    in aux count list

float : Float -> String
float = floatWithDigits 2

floatWithDigits : Int -> Float -> String
floatWithDigits digits n =
    let intPart = floor n
        intStr = int intPart
        remPart = n - toFloat intPart
        remInt = round <| remPart * toFloat (10^digits)
        remStr =
            if remInt == 0 then
                ""
            else
                "." ++
                    (toString remInt
                        |> String.toList
                        |> padToLength digits '0'
                        |> trimFromEnd '0'
                        |> String.fromList
                    )
    in intStr ++ remStr

dropWhile : (a -> Bool) -> List a -> List a
dropWhile f list =
    case list of
        [] -> []
        (x :: xs) ->
            if f x then
                dropWhile f xs
            else
                list

trimFromEnd : a -> List a -> List a
trimFromEnd x list =
    list
        |> List.reverse
        |> dropWhile ((==) x)
        |> List.reverse

padToLength : Int -> a -> List a -> List a
padToLength len x list =
    let curLen = List.length list
        padding = List.repeat (len - curLen) x
    in padding ++ list
