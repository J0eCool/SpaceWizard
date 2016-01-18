module Serialize where

import Focus exposing (..)
import Json.Decode as D exposing ((:=))
import Json.Encode as E exposing (Value)

import ListUtil exposing (findWith)

type alias Serializer a =
  { encode : a -> Value
  , decoder : D.Decoder a
  }

type alias SerializeData a b =
  (String, Focus a b, Serializer b)

encode : Serializer a -> a -> Value
encode serializer model =
  serializer.encode model

decoder : Serializer a -> D.Decoder a
decoder serializer =
  serializer.decoder

list : List (SerializeData a b) -> a -> Serializer a
list data init =
  let
    encode data model =
      data
        |> List.map (\(name, focus, serializer) ->
            E.object [("key", E.string name), ("value", serializer.encode <| get focus model)])
        |> E.list
    decoder data init =
      let
        found name =
          findWith (\(saved, _, _) -> name == saved) data
        item name =
          case found name of
            Nothing ->
              D.fail <| "Can't find key " ++ name
            Just (_, _, serializer) ->
              D.object1 (\v -> (name, v)) ("value" := serializer.decoder)
        readItem (name, value) model =
          case found name of
            Nothing ->
              model
            Just (_, focus, _) ->
              set focus value model
      in
        ("key" := D.string) `D.andThen` item
          |> D.list
          |> D.map (List.foldl readItem init)
  in
    { encode = encode data
    , decoder = decoder data init
    }

pair : (a -> Value) -> D.Decoder a -> Serializer a
pair encode decoder =
  { encode = encode, decoder = decoder }

float : Serializer Float
float =
  pair E.float D.float
