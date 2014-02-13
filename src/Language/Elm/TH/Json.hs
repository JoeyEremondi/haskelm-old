-----------------------------------------------------------------------------
--
-- Module      :  Language.Elm.TH.Json
-- Copyright   :  Copyright: (c) 2011-2013 Joey Eremondi
-- License     :  BSD3
--
-- Maintainer  :  joey.eremondi@usask.ca
-- Stability   :  experimental
-- Portability :  portable
--
-- |
--
-----------------------------------------------------------------------------
module Language.Elm.TH.Json where

{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiWayIf #-}

import Language.Haskell.TH.Syntax

import Data.Aeson.TH


import qualified SourceSyntax.Module as M
import qualified SourceSyntax.Declaration as D
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Literal as L
import qualified SourceSyntax.Location as Lo
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as T

import Data.List (isPrefixOf)

import Language.Haskell.TH.Desugar.Sweeten
import Language.Haskell.TH.Desugar

import Language.Elm.TH.Util


--import Parse.Expression (makeFunction)

import Control.Applicative

import Control.Monad.State (StateT)
import qualified Control.Monad.State as S


------------------------------------------------------------------------------------
--Helpers to make to and fromJson functions

-- | Build the AST for the base-cases, translating primitive types, lists, tuples, etc.
makeJsonCase0 (jCtor, ctorName) = Match (ConP (mkName jCtor) [] ) (NormalB $ ConE (mkName ctorName) ) [] 
makeJsonCase1 (jCtor, varName, ctorName) = Match (ConP (mkName jCtor) [VarP (mkName varName)]) (NormalB $ AppE (ConE (mkName ctorName)) (VarE (mkName varName))) [] 

-- | A list of Match values representing the "base cases" for toJson
-- | These are checked before ADT conversion is performed
unJsonCase :: [Match]
unJsonCase = map makeJsonCase1 list1 ++ map makeJsonCase0 list0 ++ [intCase]
  where
    list1 = [--("Json.Array", "lst", "FromJSON_List"), --TODO can do types?
             ( sumTypePrefix ++"_Float", "n",  "Json.Number"),
             (sumTypePrefix ++"_String", "s", "Json.String"),
             (sumTypePrefix ++"_Bool", "b", "Json.Boolean")]
    list0 = [(sumTypePrefix ++ "_Null", "Json.Null")]
    intCase = Match (ConP (mkName $ sumTypePrefix ++"_Int") [VarP (mkName "i")]) (NormalB $ AppE (ConE (mkName "Json.Number")) (AppE (VarE $ mkName "toFloat")(VarE (mkName "i")) ) ) []
    --Can't encode lists directly
    --listCase = Match (ConP (mkName "Json.Array") [VarP (mkName "l")]) (NormalB $ AppE (ConE (mkName "FromJSON_List")) (AppE (AppE (VarE (mkName "map")) (VarE (mkName "fromJson"))) (VarE (mkName "l")) )) [] 

-- | A list of Match values representing the "base cases" for fromJson
-- | These are checked before ADT conversion is attempted    
jsonCase :: [Match]
jsonCase = map makeJsonCase1 list1 ++ map makeJsonCase0 list0 ++ [listCase]
  where
    list1 = [--("Json.Array", "lst", "FromJSON_List"), --TODO can do types?
             ("Json.Number", "n", sumTypePrefix ++"_Float"),
             ("Json.String", "s", sumTypePrefix ++"_String"),
             ("Json.Boolean", "b", sumTypePrefix ++"_Bool")]
    list0 = [("Json.Null", sumTypePrefix ++"_Null")]
    listCase = Match (ConP (mkName "Json.Array") [VarP (mkName "l")]) (NormalB $ AppE (ConE (mkName $ sumTypePrefix ++"_List")) (AppE (AppE (VarE (mkName "map")) (VarE (mkName "fromJson"))) (VarE (mkName "l")) )) []     
    

-- | Filter function to test if a dec is a data
-- Also filters out decs which types that can't be serialized, such as functions
isData :: Dec -> Bool
isData dec = (isData' dec) && (canSerial dec) 
  where
    isData' DataD{} = True
    isData' NewtypeD{} = True
    isData' TySynD{} = True
    isData' _ = False
    
    canSerial (DataD _ _ _ ctors _) = all canSerialCtor ctors
    canSerial (NewtypeD _ _ _ ctor _) = canSerialCtor ctor
    canSerial (TySynD _ _ ty) = canSerialType ty
    --can't serialize if type variables --TODO is this true?
    canSerial _ = False
    
    canSerialCtor (NormalC _ types) = all (canSerialType) (map snd types)
    canSerialCtor (RecC _ types) = all (canSerialType) (map (\(_,_,c)->c) types)
    
    canSerialType (ArrowT) = False
    canSerialType t = all canSerialType (subTypes t)

-- | Expression for the fromJson function
fromJson :: Exp
fromJson = VarE (mkName "fromJson")

-- | Expression for the toJson function
toJson :: Exp
toJson = VarE (mkName "toJson")

-- | The variable representing the current Json argument
json :: Exp
json = VarE (mkName "json")

-- | Pattern for an argument named 'json'
jsonPat :: Pat
jsonPat = VarP (mkName "json") 

-- | Variable for the getter function getting the nth variable from a Json
varNamed :: Exp
varNamed = VarE (mkName "varNamed")

-- | Variable for the getter function getting the nth variable from a Json
jsonType :: Exp
jsonType = VarE (mkName "getType")

-- | Variable for the getter function getting the nth variable from a Json
jsonCtor :: Exp
jsonCtor = VarE (mkName "getCtor")

-- | Expression getting the nth subvariable from a JSON object
getVarNamed :: String -> Exp
getVarNamed nstr = AppE (AppE varNamed json ) (LitE $ StringL nstr)

-- | Expression to access the "type" field of a JSON object
getType :: Exp
getType = AppE jsonType json  

-- | Expression to access the constructor field of a JSON object
getCtor :: Exp
getCtor = AppE jsonCtor json 

-- | Expression representing function composition
fnComp :: Exp
fnComp = VarE $ mkName "."

-- | The string prefix for the massive JSON sum type
sumTypePrefix :: String
sumTypePrefix = "BoxedJson"

-- |The String argument of the massive JSON sum type property denoting a given ADT
typeString :: Name -> SQ String
typeString name = return $ sumTypePrefix ++ "_" ++  nameToString name


-- |The Pattern to unbox a value into its type from the massive sum type
-- | the second argument is the name to bind the value to
unJsonPat :: Name -> Name -> SQ Pat
unJsonPat typeName nameToBind = do
  typeCtor <- mkName <$> typeString typeName
  return $ ConP typeCtor [VarP nameToBind]

-- | The name of the constructor which wraps
-- the type with the given name into the giant sum type
sumTypeCtor :: Name -> SQ Name
sumTypeCtor name = mkName <$> typeString name

-- | Recursively generates an expression for the function which takes an argument of type BoxedJson
-- and converts it, while also extracting it from the BoxedJson type
unJsonType :: Type -> SQ Exp
unJsonType (ConT name) = do
  argName <- liftNewName "x"
  lambdaPat <- unJsonPat name argName
  let unCtor = LamE [lambdaPat] (VarE argName)
  return $ InfixE (Just unCtor) fnComp (Just fromJson)
  where
    fnComp = VarE $ mkName "."

unJsonType (AppT ListT t) = do
  subFun <- unJsonType t
  let mapVar = VarE $ mkName "mapJson"
  return $ AppE mapVar subFun


  
--Unpack JSON into a tuple type
--We convert the JSON to a list
--We make a lambda expression which applies the UnFromJSON function to each element of the tuple
unJsonType t
  | isTupleType t = do
      
      let tList = tupleTypeToList t
      let n = length tList
      --Generate the lambda to convert the list into a tuple
      subFunList <- mapM unJsonType tList
      argNames <- mapM (liftNewName . ("x" ++) . show) [1 .. n]
      let argValues = map VarE argNames
      let argPat = ListP $ map VarP argNames
      let lambdaBody = TupE $ zipWith AppE subFunList argValues
      let lambda = LamE [argPat] lambdaBody
      let makeList = VarE $ mkName "makeList"
      
      return $ InfixE (Just lambda) fnComp (Just makeList)
  --For a maybe, we construct a function that returns Nothing if it reads null
  -- or Just (unboxed fromJson val) if it is not null
  
  | isMaybeType t = do
      let (AppT _ innerT) = t
      argName <- liftNewName "maybeArg"
      subFn <- unJsonType innerT
      let nothingMatch = Match (ConP (mkName "Json.Null") []) (NormalB $ VarE $ mkName "Nothing") []
      let otherMatch = Match (WildP) (NormalB $ AppE (VarE $ mkName "Just") (AppE subFn (VarE argName))) []
      return $ LamE [VarP argName] (CaseE (VarE argName) [nothingMatch, otherMatch]) 
  | isMapType t = do
      let (AppT (AppT (ConT _name) keyT) valT) = t
      tupleFun <- unJsonType (AppT ListT (AppT (AppT (TupleT 2) keyT) valT))
      return $ InfixE (Just $ VarE $ mkName "Data.Map.fromList") fnComp (Just tupleFun) --TODO make variable
  | otherwise = do
      test <- S.lift $ isIntType t
      case test of
        True -> do
          argName <- liftNewName "x"
          lambdaPat <- unJsonPat (mkName "Int") argName
          let unCtor = LamE [lambdaPat] (AppE (VarE (mkName "round")) (VarE argName) )
          return $ InfixE (Just unCtor) fnComp (Just fromJson)
        _ -> unImplemented $ "Can't un-json type " ++ show t
        
-- | Generate a declaration, and a name bound in that declaration,
-- Which unpacks a value of the given type from the nth field of a JSON object
getSubJson :: (String, Type) -> SQ (Name, Dec)
-- We need special cases for lists and tuples, to unpack them
--TODO recursive case
getSubJson (field, t) = do
  funToApply <- unJsonType t
  subName <- liftNewName "subVar"
  let subLeftHand = VarP subName
  let subRightHand = NormalB $ AppE funToApply (getVarNamed field)
  return (subName, ValD subLeftHand subRightHand [])


-- | Given a type constructor, generate the match which matches the "ctor" field of a JSON object
-- | to apply the corresponding constructor to the proper arguments, recursively extracted from the JSON
fromMatchForCtor :: Con -> SQ Match        
fromMatchForCtor (NormalC name types) = do
  let matchPat = LitP $ StringL $ nameToString name
  (subNames, subDecs) <- unzip <$> mapM getSubJson (zip (map show [1,2..] ) (map snd types) )
  let body = NormalB $ if null subNames
              then applyArgs subNames ctorExp
              else LetE subDecs (applyArgs subNames ctorExp)
  return $ Match matchPat body []
  where
    ctorExp = ConE name
    applyArgs t accum = foldl (\ accum h -> AppE accum (VarE h)) accum t 

fromMatchForCtor (RecC name vstList) = do
  let nameTypes = map (\(a,_,b)->(nameToString a,b)) vstList
  let matchPat = LitP $ StringL $ nameToString name
  (subNames, subDecs) <- unzip <$> mapM getSubJson nameTypes
  let body = NormalB $ if null subNames
              then applyArgs subNames ctorExp
              else LetE subDecs (applyArgs subNames ctorExp)
  return $ Match matchPat body []
  where
    ctorExp = ConE name
    applyArgs t accum = foldl (\ accum h -> AppE accum (VarE h)) accum t
    
-- | Given a type delcaration, generate the match which matches the "type" field of a JSON object
-- and then defers to a case statement on constructors for that type
fromMatchForType :: Dec -> SQ Match
fromMatchForType dec@(DataD _ name _ ctors _deriving) = do
  let matchPat = LitP $ StringL $ nameToString name
  ctorMatches <- mapM fromMatchForCtor ctors
  let typeBody = NormalB $ CaseE getCtor ctorMatches
  jsonName <- liftNewName "typedJson"
  typeCtor <- sumTypeCtor name
  let typeBodyDec = ValD (VarP jsonName) typeBody []
  let ret = AppE (ConE typeCtor) (VarE jsonName)
  let body = NormalB $ LetE [typeBodyDec] ret
  return $ Match matchPat body []

fromMatchForType (NewtypeD cxt name tyBindings  ctor nameList) = 
  fromMatchForType $ DataD cxt name tyBindings [ctor] nameList  

fromMatchForType dec@(TySynD name _tyvars ty) = do
    let matchPat = WildP
    typeCtor <- sumTypeCtor name
    funToApply <- unJsonType ty
    let body = NormalB $ AppE (ConE typeCtor) (AppE (funToApply) json)
    return $ Match matchPat body []

fromMatchForType t = unImplemented $ "types other than Data, Type or Newtype " ++ show t    
  
-- |Given a list of declarations, generate the fromJSON function for all
-- types defined in the declaration list
makeFromJson :: [Dec] -> SQ [Dec]
makeFromJson allDecs = do
  let decs = filter isData allDecs
  typeMatches <- mapM fromMatchForType decs
  let objectBody = NormalB $ CaseE getType typeMatches
  let objectMatch = Match WildP objectBody []
  let body = NormalB $ CaseE json (jsonCase ++ [objectMatch])
  return [ FunD (mkName "fromJson") [Clause [jsonPat] body []] ]

  
-----------------------------------------------------------------------
-- |Given a list of declarations, generate the toJSON function for all
-- types defined in the declaration list
makeToJson :: [Dec] -> SQ [Dec]
makeToJson allDecs = do
  let decs = filter isData allDecs
  typeMatches <- mapM toMatchForType decs
  --TODO remove jsonCase, put in equivalent
  let body = NormalB $ CaseE json (unJsonCase ++ typeMatches)
  return [ FunD (mkName "toJson") [Clause [jsonPat] body []] ]

-- | Helper function to generate a the names X1 .. Xn with some prefix X  
nNames :: Int -> String -> SQ [Name]
nNames n base = do
  let varStrings = map (\n -> base ++ show n) [1..n]
  mapM liftNewName varStrings

--Generate the Match which matches against the given constructor
--then packs its argument into a JSON with the proper type, ctor and argument data
toMatchForCtor :: Name -> Con -> SQ Match        
toMatchForCtor typeName (NormalC name types) = do
  let n = length types
  adtNames <- nNames n "adtVar"
  jsonNames <- nNames n "jsonVar"
  let adtPats = map VarP adtNames
  let matchPat = ConP name adtPats
  jsonDecs <- mapM makeSubJson (zip3 (map snd types) adtNames jsonNames)
  dictName <- liftNewName "objectDict"
  dictDec <-  makeDict typeName name dictName jsonNames
  let ret = AppE (VarE $ mkName "Json.Object") (VarE dictName)
  let body = NormalB $ LetE (jsonDecs ++ [dictDec]) ret
  return $ Match matchPat body []

toMatchForCtor typeName (RecC name vstList) = do
  let (adtNames, _, types) = unzip3 vstList
  let n = length types
  jsonNames <- nNames n "jsonVar"
  let adtPats = map VarP adtNames
  let matchPat = ConP name adtPats
  jsonDecs <- mapM makeSubJson (zip3 types adtNames jsonNames)
  dictName <- liftNewName "objectDict"
  dictDec <-  makeDict typeName name dictName jsonNames
  let ret = AppE (VarE $ mkName "Json.Object") (VarE dictName)
  let body = NormalB $ LetE (jsonDecs ++ [dictDec]) ret
  return $ Match matchPat body []  

dictExp :: Name -> [Name] -> SQ Exp
dictExp ctorName jsonNames = do
  let ctorExp = LitE $ StringL $ nameToString ctorName
  let tagList = [TupE $ [LitE $ StringL "tag", AppE (VarE (mkName "Json.String")) ctorExp]]
  let elemArray = AppE (VarE $ mkName "Json.Array" ) (ListE $ map VarE jsonNames)
  let contentList = case jsonNames of
        [] -> []
        [elem] -> [TupE $ [LitE $ StringL "contents", VarE elem]]
        _ -> [TupE $ [LitE $ StringL "contents", elemArray]]
  return $ AppE (VarE $ mkName "Data.Map.fromList") (ListE $ tagList ++ contentList)

  
-- | Generate the declaration of a dictionary mapping field names to values
-- to be used with the JSON Object constructor
makeDict :: Name -> Name -> Name -> [Name] -> SQ Dec    
makeDict typeName ctorName dictName jsonNames = do
  let leftSide = VarP dictName
  rsExp <- dictExp ctorName jsonNames
  let rightSide = NormalB rsExp 
  return $ ValD leftSide rightSide []
  
 -- |Generate the Match which matches against the BoxedJson constructor
 -- to properly encode a given type
toMatchForType :: Dec -> SQ Match
toMatchForType dec@(DataD _ name _ ctors _derive) = do
  varName <- liftNewName "adt"
  matchPat <- unJsonPat name varName
  ctorMatches <- mapM (toMatchForCtor name) ctors
  let body = NormalB $ CaseE (VarE varName) ctorMatches
  return $ Match matchPat body []  

toMatchForType (NewtypeD cxt name tyBindings  ctor nameList) = 
  toMatchForType $ DataD cxt name tyBindings [ctor] nameList
  
--Type synonym, just get the unJson function, no cases to handle  
toMatchForType (TySynD name _tyVars ty) = do
    varName <- liftNewName "adt"
    matchPat <- unJsonPat name varName
    funToApply <- pureJsonType ty
    let body = NormalB $ AppE (funToApply) (VarE varName)
    return $ Match matchPat body [] 
  
-- | Generate the declaration of a value converted to Json
-- given the name of an ADT value to convert
makeSubJson :: (Type, Name, Name) -> SQ Dec
-- We need special cases for lists and tuples, to unpack them
--TODO recursive case
makeSubJson (t, adtName, jsonName) = do
  funToApply <- pureJsonType t
  let subLeftHand = VarP jsonName
  let subRightHand = NormalB $ AppE funToApply (VarE adtName)
  return $ ValD subLeftHand subRightHand []

-- | For a type, generate the expression for the function which takes a value of that type
--  and converts it to JSON
-- used to recursively convert the data of ADTs
pureJsonType :: Type -> SQ Exp
--Base case: if an ADT, just call toJson with the appropriate constructor
pureJsonType (ConT name) = do
  argName <- liftNewName "adt"
  typeCtor <- sumTypeCtor name
  lambdaPat <- unJsonPat name argName
  let addCtor = LamE [VarP argName] (AppE (ConE typeCtor) (VarE argName))
  return $ InfixE (Just toJson) fnComp (Just addCtor)
  where
    fnComp = VarE $ mkName "."

pureJsonType (AppT ListT t) = do
  subFun <- pureJsonType t
  let listCtor = VarE $ mkName "Json.Array"
  let mapVar = VarE $ mkName "map"
  return $ InfixE (Just listCtor ) fnComp (Just (AppE mapVar subFun))
  where
    fnComp = VarE $ mkName "."

--Unpack JSON into a tuple type
--We convert the JSON to a list
--We make a lambda expression which applies the UnFromJSON function to each element of the tuple
pureJsonType t
  | isTupleType t = do
      let tList = tupleTypeToList t
      let n = length tList
      --Generate the lambda to convert the list into a tuple
      subFunList <- mapM pureJsonType tList
      argNames <- mapM (liftNewName . ("x" ++) . show) [1 .. n]
      let argValues = map VarE argNames
      let argPat = TupP $ map VarP argNames
      --Get each tuple element as Json, then wrap them in a Json Array
      let listExp = AppE (VarE $ mkName "Json.Array") (ListE $ zipWith AppE subFunList argValues)
      return $ LamE [argPat] listExp    
   | isMaybeType t = do
      let (AppT _ innerT) = t
      argName <- liftNewName "maybeArg"
      justArg<- liftNewName "justArg"
      subFn <- pureJsonType innerT
      let nothingMatch = Match (ConP (mkName "Nothing") []) (NormalB $ VarE $ mkName "Json.Null") []
      let otherMatch = Match (ConP (mkName "Just") [VarP justArg]) (NormalB $ AppE subFn (VarE justArg)) []
      return $ LamE [VarP argName] (CaseE (VarE argName) [nothingMatch, otherMatch])
   | isMapType t = do
      let (AppT (AppT (ConT _name) keyT) valT) = t
      tupleFun <- pureJsonType (AppT ListT (AppT (AppT (TupleT 2) keyT) valT))
      return $ InfixE (Just tupleFun) fnComp (Just $ VarE $ mkName "Data.Map.toList") --TODO make variable
  --Don't need special int case, that happens when actually boxing the Json
-----------------------------------------------------------------------

-- | Generate a giant sum type representing all of the types within this module
-- this allows us to use toJson and fromJson without having typeClasses
giantSumType :: [Dec] -> SQ [Dec]
giantSumType allDecs = do
  let decs = filter isData allDecs
  let typeNames = map getTypeName decs ++  map mkName ["Int", "Float", "Bool", "String"] --TODO lists?
  
  ctorStrings <- mapM typeString typeNames
  let ctorNames = zip typeNames (map mkName ctorStrings)
  let nullCtor = NormalC (mkName $ sumTypePrefix ++ "_Null") []
  let listCtor = NormalC (mkName $ sumTypePrefix ++  "_List") [(NotStrict, AppT ListT (ConT $ mkName sumTypePrefix)) ]
  let ctors = map (\ (typeName, ctorName) -> NormalC ctorName [(NotStrict, ConT typeName)] ) ctorNames
  return [ DataD [] (mkName sumTypePrefix) [] (ctors ++ [nullCtor, listCtor]) [] ]
    where 
      getTypeName :: Dec -> Name
      getTypeName (DataD _ name _ _ _ ) = name
      getTypeName (NewtypeD _ name _tyBindings  _ctor _nameList) = name
      getTypeName (TySynD name _ _) = name
