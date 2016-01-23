module ListUtil where

index : Int -> List a -> Maybe a
index n list =
  List.head <| List.drop n list

indexOf : a -> List a -> Maybe Int
indexOf item list =
  indexWith (\x -> x == item) list

indexWith : (a -> Bool) -> List a -> Maybe Int
indexWith f list =
  List.indexedMap (,) list
    |> findWith (snd >> f)
    |> Maybe.map fst

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

replace : a -> a -> List a -> List a
replace x with list =
  case list of
    [] -> []
    (y :: ys) ->
      if x == y then
        with :: ys
      else
        y :: replace x with ys

mapSum : (a -> number) -> List a -> number
mapSum f list =
  list
    |> List.map f
    |> List.sum

updateIndex : Int -> (a -> a) -> List a -> List a
updateIndex n f list =
  case (n, list) of
    (_, []) ->
      []
    (0, x :: xs) ->
      f x :: xs
    (i, x :: xs) ->
      x :: updateIndex (i - 1) f xs
