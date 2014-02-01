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

import qualified Elm.Internal.Version as Version

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
               Hiding [] -> (P.text $ "import open " ++ name ++ " ")
               Hiding strs -> (P.text $ "import open " ++ name ++ " ") <+> (commaCat $ map P.text strs)

    in P.sep [modPret, importPret, decPret]  
  
                    
instance Binary ImportMethod where
    put method =
        let put' n info = putWord8 n >> put info in
        case method of
          As s         -> put' 0 s
          Importing ss -> put' 1 ss
          Hiding ss    -> put' 2 ss

    get = do tag <- getWord8
             case tag of
               0 -> As        <$> get
               1 -> Importing <$> get
               2 -> Hiding    <$> get
               _ -> error "Error reading valid ImportMethod type from serialized string"

data MetadataModule =
    MetadataModule
    { names     :: [String]
    , path      :: FilePath
    , exports   :: [String]
    , imports   :: [(String, ImportMethod)]
    , program   :: LExpr
    , types     :: Map.Map String Type
    , fixities  :: [(Assoc, Int, String)]
    , aliases   :: [Alias]
    , datatypes :: [ADT]
    } deriving Show

type Interfaces = Map.Map String ModuleInterface
type ADT = (String, [String], [(String,[Type])], [Derivation])
type Alias = (String, [String], Type, [Derivation])

data ModuleInterface = ModuleInterface {
    iVersion  :: Version.Version,
    iTypes    :: Map.Map String Type,
    iImports  :: [(String, ImportMethod)],
    iAdts     :: [ADT],
    iAliases  :: [Alias],
    iFixities :: [(Assoc, Int, String)]
} deriving Show

metaToInterface :: MetadataModule -> ModuleInterface
metaToInterface metaModule =
    ModuleInterface
    { iVersion  = Version.elmVersion
    , iTypes    = types metaModule
    , iImports  = imports metaModule
    , iAdts     = datatypes metaModule
    , iAliases  = aliases metaModule
    , iFixities = fixities metaModule
    }

instance Binary ModuleInterface where
  get = ModuleInterface <$> get <*> get <*> get <*> get <*> get <*> get
  put modul = do
      put (iVersion modul)
      put (iTypes modul)
      put (iImports modul)
      put (iAdts modul)
      put (iAliases modul)
      put (iFixities modul)
