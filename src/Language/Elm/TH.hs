{-|
Haskelm: Translate Haskell source-code into Elm source-code
via Template Haskell.

## Library

The given functions can be used to convert Haskell source code
into Elm source code.
For example:


    elmSource = $(translateToElm defaultOptions "path/to/myFile.hs")

Then, elmString1 will be a String variable which you can use in your Haskell code.
Note that the Haskell functions in the file you give are NOT imported.
If you would like to use them, you must import them the normal way.
 

## Translation

Haskelm can currently translate most basic Haskell, including functions, algebraic data types, newtypes, and type synonyms.
Support is now in place for records, guarded-function-bodies, list-ranges, where-declarations, as-patterns, 
and multi-clause function definitions (pattern matching).

Translation of class or instance declarations is not supported, and will not likely be supported in the near future,
as Elm does not support Type classes.
However, if your Haskell code contains Class or Instance declarations,
they will simply be ignored by Haskelm.

Most GHC extensions are unsupported, with the exception of Multi-Way-If statements,
since they have a direct translation into Elm.

## Json

Haskelm currently derivies toJson and fromJson functions for all Data declarations.
To get around the lack of TypeClasses in Elm, each translated module contains a 
sum type, called BoxedJson, which wraps around any types defined in the module,
as well as lists, integers, floats, bools, and null.

Values of type `FOO` can be boxed using the constructor `BoxedJsonFOO`.
This also applies to `Int`, `Float`, and `String`.
Note that `BoxedJson_List` wraps a list of type `BoxedJson`.

The Haskell versions of these functions will lbe avaliable soon.
A short-term goal of mine is to switch this format to be compatible with Aeson,
or to use a more efficient binary serialization format such as BSON
or Protocol Buffers.

Json translation can be turned off using the options parameter.
Switching off JSON translations in the Haskelm executable will be supported soon.

-}

module Language.Elm.TH
    ( 
    translateToElm,
    TranslateOptions (..),
    defaultOptions,

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

-- | Options for how to generate Elm source code
data TranslateOptions = Options {
 --
 makeJson :: Bool,
 -- ^ When true, generates `toJson` and `fromJson` for translated type declarations.
 -- The format used by the Json is the same as the one used by Data.Aeson.TH.
 -- This is handy for passing data between a Haskell server and an Elm client.
 qualifiedImports :: [String],
 -- ^ Each module name given will be imported in Elm by `import Module`
 openImports :: [String],
 -- ^ Each module name given will be imported in Elm by `import Module (..)`
 moduleName :: String
 -- ^ The name of the elm module generated. i.e. prepends `module ModuleName` to the generated Elm source.
}

{- |
Default options for translation:
Generates `toJson` and `fromJson` functions,
has no open or qualified imports, and has
module name `Main`.

-}
defaultOptions = Options True [] [] "Main"


-- | 'toElm' takes a 'String' module name and a list of Template Haskell declarations
-- and generates a translated Elm AST module
toElm :: TranslateOptions -> [Dec] -> Q (M.Module D.Declaration)
toElm options decs = do
  let doJson = makeJson options
  fromJsonDecs <- if doJson then evalStateT  (Json.makeFromJson decs) Util.defaultState else return []
  toJsonDecs <- if doJson then evalStateT  (Json.makeToJson decs) Util.defaultState else return []
  let jsonDecs = fromJsonDecs ++ toJsonDecs
  --sumDecs <- evalStateT  (Json.giantSumType decs) Util.defaultState
  elmDecs <- evalStateT  (concat <$> translateDecs (decs ++ jsonDecs)  ) Util.defaultState
  let importList = map (\im->(im, Importing [])) $ qualifiedImports options
  let openImportList = map (\im->(im, Hiding [])) $ openImports options
  return $ M.Module [moduleName options] [] (importList ++ openImportList) elmDecs 

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
      allImports = imports ++ [("Json", M.As "Json"), ("Dict", M.As "Dict"), ("JsonUtil", M.As "JsonUtil"), ("Error", M.As "Error")]
      newModule = Module [name] exports allImports allDecs
      modString = show $ Pretty.pretty newModule
  in modString              
               
    


-- | Given options for translation, and the file path of a Haskell module,
-- generate the String literal which is the corresponding Elm source code.
-- This must be invoked using Template Haskell
-- For example: 
--
-- >  elmSource = $(translateToElm defaultOptions "path/to/myFile.hs")
translateToElm :: TranslateOptions -> FilePath -> ExpQ
translateToElm options filePath = do
  decs <- decsFromModuleFile filePath
  elmString <- toElmString options decs
  liftString elmString
