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
