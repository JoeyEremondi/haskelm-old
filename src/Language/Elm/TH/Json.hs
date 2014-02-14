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

--Helper function to apply arguments to a function
applyArgs :: Exp -> [Exp] -> Exp
applyArgs fun args = foldl (\ accumFun nextArg -> AppE accumFun nextArg) fun args

--General helper functions
jsonArgName :: Name
jsonArgName = mkName "jsonArg"

jsonArgPat :: Pat
jsonArgPat = VarP jsonArgName

fromJsonName :: Name -> Name
fromJsonName name = mkName $ "fromJson_" ++ nameToString name

-- | Given a type, and an expression for an argument of type Json
-- return the expression which applies the proper fromJson function to that expression
fromJsonForType :: Type -> SQ Exp
fromJsonForType t = do
  --lookup t in state
  dec <- error "TODO implement dec lookup from type"
  return $ error "TODO implement dec lookup from type"

-- |Given a type declaration, generate the function declaration
-- Which takes a Json object to a value of that type
fromJsonForDec :: Dec -> SQ Dec

fromJsonForDec dec@(DataD _ name _ ctors _deriving) = do
  let argTagExpression = error "TODO add expr for getting tag from arg"
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
  argExps <- mapM fromJsonForType types
  let rightHandSide = NormalB $ applyArgs ctorExp argExps
  return $ Match leftHandSide rightHandSide []

fromMatchForCtor _ = error "TODO implement constructor match generation"

makeFromJson = error "TODO implement making json"

makeToJson = error "TODO implement making json"
