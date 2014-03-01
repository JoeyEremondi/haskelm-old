{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Module where

import Data.Binary
import qualified Data.Map as Map
import Control.Applicative ((<$>), (<*>))

import SourceSyntax.Expression (LExpr)
import SourceSyntax.Declaration
import SourceSyntax.Type

import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P

import Data.List (intercalate)

data Module def =
    Module [String] Exports Imports [def]
    deriving (Show)

type Exports = [String]

type Imports = [(String, ImportMethod)]
data ImportMethod = As String | Importing [String] | Hiding [String]
                    deriving (Eq, Ord, Show)

                    
instance (Pretty def ) => Pretty (Module def) where
  pretty (Module modNames exportList importList decs) = 
    let 
        exportPret = case exportList of 
                          [] -> P.text " "
                          _ -> P.parens $ commaCat $ map P.text exportList
        
        decPret = P.sep $ map pretty decs
        modName = P.text $ intercalate "." modNames
        modPret = (P.text "module" <+> modName <+> exportPret <+>  P.text "where")
        
        
        importPret = P.vcat $ map prettyImport importList
        
        prettyImport (name, method) = 
          case method of
               As s -> if name == s 
                          then P.text $ "import " ++ name 
                          else P.text $ "import " ++ name ++ " as " ++ s
               Importing strs -> (P.text $ "import " ++ name ++ " ") <+> (commaCat $ map P.text strs)
               Hiding [] -> (P.text $ "import " ++ name ++ "(..) ")
               Hiding strs -> (P.text $ "import " ++ name ++ " ") <+> P.parens (commaCat $ map P.text strs)

    in P.sep [modPret, importPret, decPret]  
  
                    



