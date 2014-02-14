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

--Helper function to apply arguments to a function
applyArgs :: Exp -> [Exp] -> Exp
applyArgs fun args = foldl (\ accumFun nextArg -> AppE accumFun nextArg) fun args


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
fromJsonForType :: Type -> Exp -> SQ Exp
fromJsonForType t = do
  --lookup t in state
  dec <- error "TODO implement dec lookup from type"
  return $ error "TODO implement dec lookup from type"

-- |Given a type declaration, generate the function declaration
-- Which takes a Json object to a value of that type
fromJsonForDec :: Dec -> SQ Dec

fromJsonForDec dec@(DataD _ name _ ctors _deriving) = do
  let argTagExpression = error "TODO add Exp for getting tag from arg"
  ctorMatches <- mapM fromMatchForCtor ctors
  let fnExp = CaseE argTagExpression ctorMatches
  let argPat = jsonArgPat
  let fnName = fromJsonName name
  let fnBody = NormalB fnExp
  let fnClause = Clause [argPat] fnBody []
  return $ FunD fnName [fnClause]

fromJsonForDec (NewtypeD cxt name tyBindings  ctor nameList) = 
  fromJsonForDec $ DataD cxt name tyBindings [ctor] nameList
  
fromMatchForCtor :: Con -> SQ Match

fromMatchForCtor (NormalC name strictTypes) = do
  let types = map snd strictTypes
  let leftHandSide = LitP $ StringL $ nameToString name
  
  let ctorExp = VarE name
  
  subDataExprs <- unpackContents jsonArgExp
  
  argExps <- zipWithM fromJsonForType types subDataExprs
  let rightHandSide = NormalB $ applyArgs ctorExp argExps
  return $ Match leftHandSide rightHandSide []

fromMatchForCtor _ = error "TODO implement constructor match generation"

unpackContents :: Exp -> SQ [Exp]
unpackContents jsonValue = error "TODO implement packing contents into json"


  
  
  
  
  

makeToJson allDecs = do
  let decs = filter isData allDecs
  mapM toJsonForDec decs

toJsonForType :: Type -> Exp -> SQ Exp
toJsonForType t = error "TODO implement toJson for types"

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
  
toMatchForCtor :: Con -> SQ Match
toMatchForCtor (NormalC name strictTypes) = do
  let types = map snd strictTypes
  let numStrings = map (("subVar_" ++) . show) [1 .. length types]
  subDataNames <- mapM liftNewName numStrings
  let subDataPats = map VarP subDataNames
  
  let leftHandSide = ConP name subDataPats
  
  let subDataExprs = map VarE subDataNames
  contentsList <- zipWithM toJsonForType types subDataExprs
  
  jsonValueExp <- packContents name contentsList
  let rightHandSide = NormalB  jsonValueExp
  
  return $ Match  leftHandSide rightHandSide []
  
packContents :: Name -> [Exp] -> SQ Exp
packContents name contentList = error "TODO implement packing contents into json"
  
  
