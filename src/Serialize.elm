module Serialize where

import Focus exposing (..)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode exposing (Value)

import ListUtil exposing (findWith)

type alias Serializer a =
  { encode : a -> Value
  , decoder : Decode.Decoder a
  }

type alias SerializeData a b =
  { data : List (String, Focus a b)
  , serializer : Serializer b
  }

encode : SerializeData a b -> a -> Value
encode serializer model =
  serializer.data
    |> List.map (\(name, focus) ->
        Encode.list [Encode.string name, serializer.serializer.encode <| get focus model])
    |> Encode.list

decoder : SerializeData a b -> a -> Decode.Decoder a
decoder serializer init =
  let
    saved =
      serializer.serializer.decoder
    readItem (name, value) model =
      let
        found =
          findWith (\(saved, _) -> name == saved) serializer.data
      in
        case found of
          Nothing ->
            model
          Just (_, focus) ->
            set focus value model
  in
    Decode.tuple2 (,) Decode.string saved
      |> Decode.list
      |> Decode.map (List.foldl readItem init)

serialize : (a -> Value) -> Decode.Decoder a -> Serializer a
serialize encode decoder =
  { encode = encode, decoder = decoder }

float : Serializer Float
float =
  serialize Encode.float Decode.float