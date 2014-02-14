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

import Control.Monad

import Control.Monad.State (StateT)
import qualified Control.Monad.State as S

-- |Helper function to apply arguments to a function
applyArgs :: Exp -> [Exp] -> Exp
applyArgs fun args = foldl (\ accumFun nextArg -> AppE accumFun nextArg) fun args

fnComp = VarE $ mkName "."

-- | Helper function to generate a the names X1 .. Xn with some prefix X  
nNames :: Int -> String -> SQ [Name]
nNames n base = do
  let varStrings = map (\n -> base ++ show n) [1..n]
  mapM liftNewName varStrings

-- | Variable for the getter function getting the nth variable from a Json
varNamed :: Exp
varNamed = VarE (mkName "JsonUtil.varNamed")
  
-- | Expression getting a named subvariable from a JSON object
getVarNamed :: String -> Exp
getVarNamed nstr = AppE (AppE varNamed jsonArgExp ) (LitE $ StringL nstr)

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

--General helper functions
jsonArgName :: Name
jsonArgName = mkName "jsonArg"

jsonArgPat :: Pat
jsonArgPat = VarP jsonArgName

jsonArgExp :: Exp
jsonArgExp = VarE jsonArgName

fromJsonName :: Name -> Name
fromJsonName name = mkName $ "toJson_" ++ nameToString name

toJsonName :: Name -> Name
toJsonName name = mkName $ "toJson_" ++ nameToString name


makeFromJson :: [Dec] -> SQ [Dec]
makeFromJson allDecs = do
  let decs = filter isData allDecs
  mapM fromJsonForDec decs

-- | Given a type, and an expression for an argument of type Json
-- return the expression which applies the proper fromJson function to that expression
fromJsonForType :: Type -> SQ Exp

--Type name not covered by Prelude
fromJsonForType (ConT name) = case (nameToString name) of
  "Int" -> return $ VarE $ mkName "JsonUtil.intFromJson"
  "Int" -> return $ VarE $ mkName "JsonUtil.floatFromJson"
  "String" -> return $ VarE $ mkName "JsonUtil.stringFromJson"
  _ -> return $ VarE $ fromJsonName name

fromJsonForType (AppT ListT t) = do
  subExp <- fromJsonForType t
  return $ AppE (VarE $ mkName "JsonUtil.listFromJson") subExp  
  
fromJsonForType (AppT (ConT name) t) = do
  subExp <- fromJsonForType t
  case (nameToString name) of
    "Maybe" -> return $ AppE (VarE $ mkName "JsonUtil.maybeFromJson") subExp
    
fromJsonForType t
  | isTupleType t = do
      let tList = tupleTypeToList t
      let n = length tList
      --Generate the lambda to convert the list into a tuple
      subFunList <- mapM fromJsonForType tList
      argNames <- mapM (liftNewName . ("x" ++) . show) [1 .. n]
      let argValues = map VarE argNames
      let argPat = ListP $ map VarP argNames
      let lambdaBody = TupE $ zipWith AppE subFunList argValues
      let lambda = LamE [argPat] lambdaBody
      let makeList = VarE $ mkName "makeList"
      return $ InfixE (Just lambda) fnComp (Just makeList)

-- |Given a type declaration, generate the function declaration
-- Which takes a Json object to a value of that type
fromJsonForDec :: Dec -> SQ Dec

fromJsonForDec dec@(DataD _ name _ ctors _deriving) = do
  let argTagExpression = AppE (VarE $ mkName "JsonUtil.getTag") jsonArgExp
  ctorMatches <- mapM fromMatchForCtor ctors
  let fnExp = CaseE argTagExpression ctorMatches
  let argPat = jsonArgPat
  let fnName = fromJsonName name
  let fnBody = NormalB fnExp
  let fnClause = Clause [argPat] fnBody []
  return $ FunD fnName [fnClause]

fromJsonForDec (NewtypeD cxt name tyBindings  ctor nameList) = 
  fromJsonForDec $ DataD cxt name tyBindings [ctor] nameList
  
fromJsonForDec dec@(TySynD name _tyvars ty) = do
  let fnName = fromJsonName name
  fnBody <- NormalB <$> fromJsonForType ty
  let fnClause = Clause [jsonArgPat] fnBody []
  return $ FunD fnName [fnClause]
  
  
fromMatchForCtor :: Con -> SQ Match

fromMatchForCtor (NormalC name strictTypes) = do
  let types = map snd strictTypes
  let leftHandSide = LitP $ StringL $ nameToString name
  
  let ctorExp = VarE name
  
  --Exp in TH, list in Haskell
  contentListExpr <- unpackContents jsonArgExp
  
  fromJsonFunctions <- mapM fromJsonForType types
  let intNames = map show [1 .. length types]
  subDataNames <- mapM liftNewName intNames
  --We unpack each json var into its own named variable, so we can unpack them into different types
  let subDataListPattern = ListP $ map VarP subDataNames
  
  let unJsonedExprList = zipWith AppE fromJsonFunctions (map VarE subDataNames)
  
  let rightHandSide = NormalB $ applyArgs ctorExp unJsonedExprList
  return $ Match leftHandSide rightHandSide []
  

fromMatchForCtor (RecC name vstList) = do
  let nameTypes = map (\(a,_,b)->(nameToString a,b)) vstList
  let matchPat = LitP $ StringL $ nameToString name
  (subNames, subDecs) <- unzip <$> mapM getSubJsonRecord nameTypes
  let body = NormalB $ if null subNames
              then applyArgs subNames ctorExp
              else LetE subDecs (applyArgs subNames ctorExp)
  return $ Match matchPat body []
  where
    ctorExp = ConE name
    applyArgs t accum = foldl (\ accum h -> AppE accum (VarE h)) accum t
    

-- | Generate a declaration, and a name bound in that declaration,
-- Which unpacks a value of the given type from the nth field of a JSON object
getSubJsonRecord :: (String, Type) -> SQ (Name, Dec)
-- We need special cases for lists and tuples, to unpack them
--TODO recursive case
getSubJsonRecord (field, t) = do
  funToApply <- fromJsonForType t
  subName <- liftNewName "subVar"
  let subLeftHand = VarP subName
  let subRightHand = NormalB $ AppE funToApply (getVarNamed field)
  return (subName, ValD subLeftHand subRightHand [])
    
unpackContents :: Exp -> SQ Exp
unpackContents jsonValue = return $ AppE (VarE $ mkName "JsonUtil.unpackContents") jsonValue


  
  
  
  
  

makeToJson allDecs = do
  let decs = filter isData allDecs
  mapM toJsonForDec decs

toJsonForType :: Type -> SQ Exp
toJsonForType (ConT name) = case (nameToString name) of
  "Int" -> return $ VarE $ mkName "JsonUtil.intToJson"
  "Int" -> return $ VarE $ mkName "JsonUtil.floatToJson"
  "String" -> return $ VarE $ mkName "JsonUtil.stringToJson"
  _ -> return $ VarE $ toJsonName name
  
toJsonForType (AppT ListT t) = do
  subExp <- toJsonForType t
  return $ AppE (VarE $ mkName "JsonUtil.listToJson") subExp  
  
toJsonForType (AppT (ConT name) t) = do
  subExp <- toJsonForType t
  case (nameToString name) of
    "Maybe" -> return $ AppE (VarE $ mkName "JsonUtil.maybeToJson") subExp

toJsonForType t 
  | isTupleType t = do
      let tList = tupleTypeToList t
      let n = length tList
      --Generate the lambda to convert the list into a tuple
      subFunList <- mapM toJsonForType tList
      argNames <- mapM (liftNewName . ("x" ++) . show) [1 .. n]
      let argValues = map VarE argNames
      let argPat = TupP $ map VarP argNames
      --Get each tuple element as Json, then wrap them in a Json Array
      let listExp = AppE (VarE $ mkName "Json.Array") (ListE $ zipWith AppE subFunList argValues)
      return $ LamE [argPat] listExp  

toJsonForDec :: Dec -> SQ Dec
toJsonForDec dec@(DataD _ name _ ctors _deriving) = do
  let argPat = jsonArgPat
  let argExp = jsonArgExp
  ctorMatches <- mapM toMatchForCtor ctors
  
  let fnExp = CaseE jsonArgExp ctorMatches
  
  let fnName = toJsonName name
  let fnBody = NormalB fnExp
  let fnClause = Clause [argPat] fnBody []
  return $ FunD fnName [fnClause]
  
toJsonForDec (NewtypeD cxt name tyBindings  ctor nameList) = 
  toJsonForDec $ DataD cxt name tyBindings [ctor] nameList
  
toJsonForDec dec@(TySynD name _tyvars ty) = do
  let fnName = toJsonName name
  fnBody <- NormalB <$> toJsonForType ty
  let fnClause = Clause [jsonArgPat] fnBody []
  return $ FunD fnName [fnClause]
 
toJsonForDec dec = error $ "Unknown dec type" ++ (show dec)

  
toMatchForCtor :: Con -> SQ Match
toMatchForCtor (NormalC name strictTypes) = do
  let types = map snd strictTypes
  let numStrings = map (("subVar_" ++) . show) [1 .. length types]
  subDataNames <- mapM liftNewName numStrings
  let subDataPats = map VarP subDataNames
  
  let leftHandSide = ConP name subDataPats
  
  let subDataExprs = map VarE subDataNames
  
  toJsonFunctions <- mapM toJsonForType types
  
  let contentsList = ListE $ zipWith AppE toJsonFunctions subDataExprs
  
  jsonValueExp <- packContents name contentsList
  let rightHandSide = NormalB  jsonValueExp
  
  return $ Match  leftHandSide rightHandSide []

toMatchForCtor (RecC name vstList) = do
  let (adtNames, _, types) = unzip3 vstList
  let n = length types
  jsonNames <- nNames n "jsonVar"
  let adtPats = map VarP adtNames
  let matchPat = ConP name adtPats
  jsonDecs <- mapM makeSubJsonRecord (zip3 types adtNames jsonNames)
  dictName <- liftNewName "objectDict"
  dictDec <-  makeRecordDict name dictName jsonNames
  let ret = AppE (VarE $ mkName "Json.Object") (VarE dictName)
  let body = NormalB $ LetE (jsonDecs ++ [dictDec]) ret
  return $ Match matchPat body []
 
-- | Generate the declaration of a dictionary mapping field names to values
-- to be used with the JSON Object constructor
makeRecordDict :: Name -> Name -> [Name] -> SQ Dec
makeRecordDict ctorName dictName jsonNames = do
  let leftSide = VarP dictName
  let jsonExps = map VarE jsonNames
  let fieldNames = map (LitE . StringL . show) [1 .. (length jsonNames)]
  let tuples = map (\(field, json) -> TupE [field, json]) (zip fieldNames jsonExps)

  let ctorExp = LitE $ StringL $ nameToString ctorName

  let ctorTuple = TupE [LitE $ StringL "tag", AppE (VarE (mkName "Json.String")) ctorExp ]
  let tupleList = ListE $ [ctorTuple] ++ tuples
  let rightSide = NormalB $ AppE (VarE $ mkName "Data.Map.fromList") tupleList
  return $ ValD leftSide rightSide []
  

-- | Generate the declaration of a value converted to Json
-- given the name of an ADT value to convert
makeSubJsonRecord :: (Type, Name, Name) -> SQ Dec
-- We need special cases for lists and tuples, to unpack them
--TODO recursive case
makeSubJsonRecord (t, adtName, jsonName) = do
  funToApply <- toJsonForType t
  let subLeftHand = VarP jsonName
  let subRightHand = NormalB $ AppE funToApply (VarE adtName)
  return $ ValD subLeftHand subRightHand []
  
packContents :: Name -> Exp -> SQ Exp
packContents name contentList = do
  return $ applyArgs (VarE $ mkName "JsonUtil.packContents") [LitE $ StringL $ nameToString name, contentList]
  
  
