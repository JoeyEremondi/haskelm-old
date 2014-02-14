module JsonUtil where

import Json

import Dict

packContents : String -> [Json.JsonValue] -> Json.JsonValue
packContents name contentList = 
  let
    dictList = [("tag", Json.String name), ("contents", Json.Array contentList)]
  in Json.Object <| Dict.fromList dictList
    
unpackContents : Json.JsonValue -> [Json.JsonValue]
unpackContents (Json.Object valDict) = case (Dict.lookup "contents" valDict) of
  Just (Json.Array contents) -> contents
  
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
intFromJson (Number f) = round f

intToJson : ToJson Int
intToJson i = Json.Number <| toFloat i

floatFromJson : FromJson Float
floatFromJson (Json.Number f) = f

floatToJson : ToJson Float
floatToJson = Json.Number

stringFromJson : FromJson String
stringFromJson (String s) = s

stringToJson : FromJson String
stringToJson = Json.String
