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

--General helper functions
jsonArgName :: Name
jsonArgName = mkName "jsonArg"

jsonArgPat :: Pat
jsonArgPat = VarP jsonArgName

fromJsonName :: Name -> Name
fromJsonName name = mkName $ "fromJson_" ++ nameToString name


fromJsonForType :: Type -> SQ Dec
fromJsonForType t = do
  --lookup t in state
  dec <- error "TODO implement dec lookup from type"
  fromJsonForDec dec

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
  
fromMatchForCtor :: Con -> SQ Match
fromMatchForCtor _ = error "TODO implement constructor match generation"

makeFromJson = error "TODO implement making json"

makeToJson = error "TODO implement making json"
