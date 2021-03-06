module Format (..) where

import String
import Currency
import ListUtil


int : Int -> String
int n =
  toString n
    |> String.toList
    |> List.reverse
    |> ListUtil.intersperseBy 3 ','
    |> List.reverse
    |> String.fromList


float : Float -> String
float =
  floatWithDigits 2


floatWithDigits : Int -> Float -> String
floatWithDigits digits n =
  let
    intPart =
      floor n

    intStr =
      int intPart

    remPart =
      n - toFloat intPart

    remInt =
      round <| remPart * toFloat (10 ^ digits)

    remStr =
      if remInt == 0 then
        ""
      else
        "."
          ++ (toString remInt
                |> String.toList
                |> ListUtil.padToLength digits '0'
                |> ListUtil.trimFromEnd '0'
                |> String.fromList
             )
  in
    intStr ++ remStr


currencyWith : (a -> String) -> ( Currency.Type, a ) -> String
currencyWith f ( t, amount ) =
  f amount ++ " " ++ Currency.abbreviation t


currency : Currency.Bundle -> String
currency =
  currencyWith int


currencyList : List Currency.Bundle -> String
currencyList list =
  list
    |> List.map currency
    |> List.intersperse ", "
    |> String.concat


floatCurrency : Currency.FloatBundle -> String
floatCurrency =
  currencyWith float
