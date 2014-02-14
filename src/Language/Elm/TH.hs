{-|
## Library
You can also use Haskelm within a Haskell program, via Template Haskell.
These functions are delcared in Language.Elm.TH

There are two stages to translation: converting a Haskell file into a list of
Template Haskell declarations (type DecsQ),
and translation those declarations.

There are 5 ways you can get Haskell declarations
1. Using TemplateHaskell [d| ... |] brackets
2. From a string which contains a list of declarations (no `module` or `import` statements)
3. From a file containin declarations as in (2)
4. From a string which contains a Haskell module (`module` and `import` statements are discarded but allowed)
5. From a file containing a module as in (4)

It's reccomended that you use (5) for files which are already in your
Haskell project, and that whenever you use (4) or (5), you do NOT
splice the Haskell declarations into your code (see below).
The imports are ignored, so this is ideal for simply reading in a Haskell
file which gets compiled into your project (without Template Haskell).

If you would like to simultaneously add Haskell and Elm definitions to
your project, you should use (1), (2) or (3), since they will read in declarations
without any import or module statements. You can then use `declareTranslation`
with `declareHaskell=True` to splice the Haskell definitions in, as well as
a definition for a variable containing the translated Elm string.

Once you have a list of declarations, you can then translate them into elm.
To translate them as an expression, use

    elmString1 = $(elmStringExp defaultOptions $ decsFromModuleFile "myfile.hs")

Then, elmString1 will be a String variable which you can use in your Haskell code.
Note that the Haskell declarations can NOT be spliced into code using this method,
even if the declareHaskell option is set to True.
 
To simultaneously declare Haskell and your translated Elm, use
  $(declareTranslation defaultOptions $ decsFromFile "mydecs.hs")
  
In this case, the Haskell declarations can refer to anything imported by
the module in which you call declareTranslation. Thus it is reccomended that
you don't use `decsFromModuleFile` or `decsFromModule`, since any imports will be discarded.
 
Note that in either case,
`defaultOptions` is a record, so you can modify any of its values in the call.


## Translation

Haskelm can currently translate most basic Haskell, including functions, algebraic data types, newtypes, and type synonyms.
Support is now in place for records, guarded-function-bodies, list-ranges, where-declarations, as-patterns, 
and multi-clause function definitions (pattern matching).

Translation of class or instance declarations is not supported, and will not likely be supported in the near future,
as Elm does not support Type classes.

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
    declareTranslation,
    elmStringExp,
    decsFromString,
    decsFromFile,
    TranslateOptions (..),
    defaultOptions,
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

defaultOptions = Options True False [] "Main" "var"


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
  return $ M.Module [moduleName options] [] (map (\im->(im, Importing [])) $ elmImports options) elmDecs 

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
      allImports = imports ++ [("Json", M.As "Json"), ("Dict", M.As "Dict"), ("JsonUtil", M.As "JsonUtil")]
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
elmStringExp options decsQ = do
  decs <- decsQ
  elmString <- toElmString options decs
  liftString elmString
