module JsonUtil where

import Json
import Error

import Dict

packContents : Int -> String -> [Json.JsonValue] -> Json.JsonValue
packContents numCtors name contentList =
  case contentList of
    -- [] -> Json.Null TODO special case for only string
    [item] -> let
          dictList = [("tag", Json.String name), ("contents", item)]
        in Json.Object <| Dict.fromList dictList
    _ ->
      if (numCtors == 0) 
        then Json.Array contentList  
      else
        let
          dictList = [("tag", Json.String name), ("contents", Json.Array contentList)]
        in Json.Object <| Dict.fromList dictList
    
unpackContents : Int -> Json.JsonValue -> [Json.JsonValue]
unpackContents numCtors json = case (json, numCtors) of
   (Json.Array contents, 0) -> contents
   --Case when there are no values, just constructor
   (Json.String s, _) -> []
   (Json.Object valDict, _) -> case (Dict.lookup "contents" valDict) of
      Just (Json.Array contents) -> contents
      --any other case, means we had a single element for contents
      Just json -> [json]
      _ -> Error.raise <| "No contents field of JSON " ++ (show json)
   _ -> Error.raise <| "No contents field of JSON. num: " ++ (show numCtors) ++ " json " ++ (show json)
  
type ToJson a = (a -> Json.JsonValue)

type FromJson a = (Json.JsonValue -> a)
  
listToJson : ToJson a -> ToJson [a]
listToJson toJson = \values -> Json.Array (map toJson values)

maybeToJson : ToJson a -> ToJson (Maybe a)
maybeToJson toJson = \mval -> case mval of
  Nothing -> Json.Null
  Just a -> toJson a
  

listFromJson : FromJson a -> FromJson [a]
listFromJson fromJson = \(Json.Array elems) -> map fromJson elems

maybeFromJson : FromJson a -> FromJson (Maybe a)
maybeFromJson fromJson = \json -> case json of
  Json.Null -> Nothing
  _ -> Just <| fromJson json
  
intFromJson : FromJson Int
intFromJson (Json.Number f) = round f

intToJson : ToJson Int
intToJson i = Json.Number <| toFloat i

floatFromJson : FromJson Float
floatFromJson (Json.Number f) = f

floatToJson : ToJson Float
floatToJson = Json.Number

stringFromJson : FromJson String
stringFromJson (Json.String s) = s

stringToJson : ToJson String
stringToJson s = Json.String s

boolFromJson : FromJson Bool
boolFromJson (Json.Boolean b) = b 

boolToJson : ToJson Bool
boolToJson b = Json.Boolean b

dictFromJson : FromJson comparable -> FromJson b -> FromJson (Dict.Dict comparable b)
dictFromJson keyFrom valueFrom = \(Json.Array tuples) ->
  let unJsonTuples = map (\ (Json.Array [kj,vj]) -> (keyFrom kj, valueFrom vj)) tuples 
  in Dict.fromList unJsonTuples
  
dictToJson : ToJson comparable -> ToJson b -> ToJson (Dict.Dict comparable b)
dictToJson keyTo valueTo = \dict ->
  let 
    dictList = Dict.toList dict
    tupleJson = map (\(k,v) -> Json.Array [keyTo k, valueTo v]) dictList 
  in Json.Array tupleJson

getTag : Json.JsonValue -> String
getTag json = case json of
  (Json.Object dict) -> case (Dict.lookup "tag" dict) of
    Just (Json.String s) -> s
    _ -> Error.raise <| "Couldn't get tag from JSON" ++ (show dict)
  (Json.String s) -> s --Ctors with no contents get stored as strings
  
varNamed : Json.JsonValue -> String -> Json.JsonValue
varNamed (Json.Object dict) name = case (Dict.lookup name dict) of
  Just j -> j
