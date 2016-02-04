module MaybeUtil (..) where


isJust : Maybe a -> Bool
isJust maybe =
  case maybe of
    Just _ ->
      True

    Nothing ->
      False


isNothing : Maybe a -> Bool
isNothing maybe =
  not (isJust maybe)


isOk : Result a b -> Bool
isOk res =
  case res of
    Ok _ ->
      True

    Err _ ->
      False
