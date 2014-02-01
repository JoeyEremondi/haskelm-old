

module Language.Elm.TH
    ( 
    declareTranslation,
    elmStringExp,
    decsFromString,
    decsFromFile,
    TranslateOptions (..),
    decsFromModuleString,
    decsFromModuleFile,
    toElmString

    ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import qualified Data.Text as TS
import SourceSyntax.Declaration as D
import SourceSyntax.Module as M
import Language.Elm.TH.BaseDecs
import Language.Haskell.TH.Lib
import qualified Language.Elm.TH.HToE as HToE
import qualified Language.Elm.TH.Json as Json
import qualified Language.Elm.TH.Util as Util
import Data.List (intercalate)
import SourceSyntax.PrettyPrint as Pretty
import Control.Monad.State (evalStateT)
import Control.Applicative ((<$>))
--source parser
import Language.Haskell.Meta.Parse
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Exts.Syntax as Exts

data TranslateOptions = Options {
 makeJson :: Bool,
 declareHaskell :: Bool,
 elmImports :: [String],
 moduleName :: String,
 varName :: String 
 
}


-- | 'toElm' takes a 'String' module name and a list of Template Haskell declarations
-- and generates a translated Elm AST module
toElm :: TranslateOptions -> [Dec] -> Q (M.Module D.Declaration)
toElm options decs = do
  let doJson = makeJson options
  fromJsonDecs <- if doJson then evalStateT  (Json.makeFromJson decs) Util.defaultState else return []
  toJsonDecs <- if doJson then evalStateT  (Json.makeToJson decs) Util.defaultState else return []
  let jsonDecs = fromJsonDecs ++ toJsonDecs
  sumDecs <- evalStateT  (Json.giantSumType decs) Util.defaultState
  elmDecs <- evalStateT  (concat <$> translateDecs (decs ++ jsonDecs ++ sumDecs)  ) Util.defaultState
  return $ M.Module [varName options] [] (map (\im->(im, Importing [])) $ elmImports options) elmDecs 

--Single stateful computation to store record state information  
translateDecs decs = do
  HToE.findRecords decs
  mapM HToE.translateDec decs
  
-- | Given a module name and a list of template-haskell declarations
-- | translate the declarations into Elm and return the string of the translated module
toElmString :: TranslateOptions -> [Dec] -> Q String
toElmString options decs = elmModuleToString <$> toElm options decs
  


-- | Translate a Haskell string into a list of Template-Haskell declarations
decsFromString :: String -> Q [Dec]
decsFromString s = case parseDecs s of
    Left e -> error $ "Failed to parse module\n" ++ e
    Right decs -> return decs


-- | Given a file containing Haskell declarations, splice them and
-- into the haskell code, while also translating them into an Elm module
decsFromFile :: String -> DecsQ
decsFromFile filePath = do
  decString <- runIO $ readFile filePath
  decsFromString decString
  
--TODO also generate options?
decsFromModuleString :: String -> DecsQ
decsFromModuleString source = case parseHsModule source of
    Left e -> error $ "Failed to parse module\n" ++ e
    Right (Exts.Module _ _ _ _ _ _ decs) -> do
      let decString = intercalate "\n" $ map prettyPrint decs
      decsFromString decString

decsFromModuleFile :: String -> DecsQ
decsFromModuleFile filePath = do
  decString <- runIO $ readFile filePath
  decsFromModuleString decString



elmModuleToString (Module [name] exports imports elmDecs ) =
  let allDecs = baseDecs ++ elmDecs 
      allImports = imports ++ [("Json", M.As "Json"), ("Dict", M.As "Dict")]
      newModule = Module [name] exports allImports allDecs
      modString = show $ Pretty.pretty newModule
  in modString              
               
-- | Given haskell declarations wrapped in '[d| ... |]', splice them and
-- into the haskell code, while also translating them into an Elm module
-- stored with the given varName
declareTranslation :: TranslateOptions -> DecsQ -> DecsQ
declareTranslation options dq = do
    decs <- dq
    elmString <- toElmString options decs
    let elmExp = liftString elmString
    let pat = varP (mkName $ varName options)
    let body = normalB elmExp
    elmDec <- valD pat body []
    --let modul = moduleFromString (TS.pack $ moduleName options) (TS.pack elmString )
    --js <- runIO $ buildModules modul []

    return $ if (declareHaskell options) then decs ++ [elmDec] else [elmDec]
    

elmStringExp :: TranslateOptions -> DecsQ -> ExpQ
elmStringExp options decs = error "TODO"