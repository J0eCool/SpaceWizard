module ListUtil where

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
  let
    curLen = List.length list
    padding = List.repeat (len - curLen) x
  in padding ++ list

findWith : (a -> Bool) -> List a -> Maybe a
findWith f list =
  case list of
    [] ->
      Nothing
    (x :: xs) ->
      if f x then
        Just x
      else
        findWith f xs

contains : a -> List a -> Bool
contains x list =
  case findWith ((==) x) list of
    Just y ->
      True
    Nothing ->
      False

remove : a -> List a -> List a
remove x list =
  case list of
    [] -> []
    (y :: ys) ->
      if x == y then
        ys
      else
        y :: remove x ys

replaceFirst : a -> a -> List a -> List a
replaceFirst x with list =
  case list of
    [] -> []
    (y :: ys) ->
      if x == y then
        with :: ys
      else
        y :: replaceFirst x with ys

mapSum : (a -> number) -> List a -> number
mapSum f list =
  list
    |> List.map f
    |> List.sum
