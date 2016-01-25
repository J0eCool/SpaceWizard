module Serialize where

import Focus exposing (..)
import Json.Decode as D exposing ((:=))
import Json.Encode as E exposing (Value)

import ListUtil exposing (index, findWith, indexWith, updateIndex)
import Operators exposing ((?>))

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

foldList : List (SerializeData a b) -> a -> Serializer a
foldList data init =
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

int : Serializer Int
int =
  pair E.int D.int

float : Serializer Float
float =
  pair E.float D.float

list : Serializer a -> Serializer (List a)
list serializer =
  let
    encode list =
      E.list <| List.map serializer.encode list
    decoder =
      D.list serializer.decoder
  in
    pair encode decoder

tuple2 : Serializer a -> Serializer b -> Serializer (a, b)
tuple2 first second =
  let
    encode (x, y) =
      E.list [first.encode x, second.encode y]
    decoder =
      D.tuple2 (,) first.decoder second.decoder
  in
    pair encode decoder

encodeObject : (SerializeData a b) -> a -> (String, Value)
encodeObject (name, focus, serializer) model =
  (name, serializer.encode <| get focus model)

object1 : m -> SerializeData m a -> Serializer m
object1 init a =
  let
    fo (_, focus, _) = focus
    de (name, _, serializer) = (name := serializer.decoder)
    en = encodeObject
  in
    { encode = \m -> E.object [en a m]
    , decoder = D.object1
        (\x ->
          set (fo a) x
            <| init)
        (de a)
    }

object2 : m -> SerializeData m a -> SerializeData m b -> Serializer m
object2 init a b =
  let
    fo (_, focus, _) = focus
    de (name, _, serializer) = (name := serializer.decoder)
    en = encodeObject
  in
    { encode = \m -> E.object [en a m, en b m]
    , decoder = D.object2
        (\x y ->
          set (fo a) x
            <| set (fo b) y
            <| init)
        (de a) (de b)
    }

object3 : m -> SerializeData m a -> SerializeData m b -> SerializeData m c -> Serializer m
object3 init a b c =
  let
    fo (_, focus, _) = focus
    de (name, _, serializer) = (name := serializer.decoder)
    en = encodeObject
  in
    { encode = \m -> E.object [en a m, en b m, en c m]
    , decoder = D.object3
        (\x y z ->
          set (fo a) x
            <| set (fo b) y
            <| set (fo c) z
            <| init)
        (de a) (de b) (de c)
    }

object4 : m -> SerializeData m a -> SerializeData m b -> SerializeData m c ->
  SerializeData m d -> Serializer m
object4 init a b c d =
  let
    fo (_, focus, _) = focus
    de (name, _, serializer) = (name := serializer.decoder)
    en = encodeObject
  in
    { encode = \m -> E.object [en a m, en b m, en c m, en d m]
    , decoder = D.object4
        (\x y z i ->
          set (fo a) x
            <| set (fo b) y
            <| set (fo c) z
            <| set (fo d) i
            <| init)
        (de a) (de b) (de c) (de d)
    }

object5 : m -> SerializeData m a -> SerializeData m b -> SerializeData m c ->
  SerializeData m d -> SerializeData m e -> Serializer m
object5 init a b c d e =
  let
    fo (_, focus, _) = focus
    de (name, _, serializer) = (name := serializer.decoder)
    en = encodeObject
  in
    { encode = \m -> E.object [en a m, en b m, en c m, en d m, en e m]
    , decoder = D.object5
        (\x y z i j ->
          set (fo a) x
            <| set (fo b) y
            <| set (fo c) z
            <| set (fo d) i
            <| set (fo e) j
            <| init)
        (de a) (de b) (de c) (de d) (de e)
    }

type alias Named a =
  { a | name : String }

namedListFocus : (b -> List (Named a)) -> (List (Named a) -> b -> b) -> b -> Named a -> Focus b (Named a)
namedListFocus get set model item =
  let
    idx =
      indexWith (\a -> a.name == item.name) (get model)
    getter m =
      idx ?> (\i -> index i (get m))
        |> Maybe.withDefault item
    updater f m =
      case idx of
        Just i ->
          let updated = updateIndex i f (get m)
          in set updated m
        Nothing ->
          m
  in
    create getter updater

namedStringList : List (Named a) -> Serializer (Named a)
namedStringList list =
  let
    encode item =
      E.string item.name
    decoder =
      D.string `D.andThen` (\name ->
        case findWith (\item -> name == item.name) list of
          Just item ->
            D.succeed item
          Nothing ->
            D.fail <| "Unexpected type " ++ name
      )
  in
    pair encode decoder
