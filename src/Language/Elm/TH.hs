-- | This module contains Shakespearean (see "Text.Shakespeare") templates for Elm.
-- It introduces type-safe compile-time variable and URL interpolation. A typeclass
-- @'ToElm'@ is provided for interpolated variables.
--
-- Further reading on Shakespearean templates: <http://www.yesodweb.com/book/templates>
--
-- Further reading on Elm: <http://elm-lang.org>

{-
In order to support modules, we'll assume there's a single directory
with a given path-name, all the elm-files
-}

module Language.Elm.TH
    ( -- * Functions
      -- ** Template-Reading Functions
      -- |These QuasiQuoters return functions of the type @(t -> 'Elm')@
      -- where @t@ is the URL rendering function if type-safe URLs are used.
      --
      -- A usage example for both type-safe (Yesod) and standard path segment (Happstack)
      -- URLs is provided in the Examples folder in the Git repository.
      --elm
    --, elmFile
    -- , elmFileReload

      -- * Datatypes
    --, Elm (..)

      -- * Typeclass for interpolated variables
    --, ToElm (..)

      -- ** Rendering Functions
    --, renderElm
    decHaskAndElm,
    decsFromString,
    decsFromFile,
    stringToDecs,
    toElmString

    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..), dataToExpQ)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import Data.Monoid
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Shakespeare

import SourceSyntax.Declaration as D
import SourceSyntax.Module as M

import Language.Haskell.TH.Lib

import qualified Language.Elm.TH.HToE as HToE
import qualified Language.Elm.TH.Json as Json
import qualified Language.Elm.TH.Util as Util

import qualified Data.Map as Map
import Data.List (intercalate)

import SourceSyntax.PrettyPrint as Pretty

--source parser
import Language.Haskell.Meta.Parse

import Language.Elm.Build (buildModules, moduleFromString)

import System.Directory

import Control.Applicative ((<$>))

import Control.Monad.State (evalStateT)

-- | General error function for unimplemented features
unImplemented s = error $ "Translation of the The following haskell feature is not yet implemented: " ++ s


-- | 'toElm' takes a 'String' module name and a list of Template Haskell declarations
-- and generates a translated Elm AST module
toElm :: String -> [Dec] -> Q (M.Module D.Declaration)
toElm name decs = do
  fromJsonDecs <- evalStateT  (Json.makeFromJson decs) Util.defaultState
  toJsonDecs <- evalStateT  (Json.makeToJson decs) Util.defaultState
  let jsonDecs = fromJsonDecs ++ toJsonDecs
  sumDecs <- evalStateT  (Json.giantSumType decs) Util.defaultState
  elmDecs <- evalStateT  (concat <$> translateDecs (decs ++ jsonDecs ++ sumDecs)  ) Util.defaultState
  return $ M.Module [name] [] [] elmDecs --TODO imports/exports?

--Single stateful computation to store record state information  
translateDecs decs = do
  HToE.findRecords decs
  mapM HToE.translateDec decs
  
-- | Given a module name and a list of template-haskell declarations
-- | translate the declarations into Elm and return the string of the translated module
toElmString :: String -> [Dec] -> Q String
toElmString name decs = moduleToString <$> toElm name decs
  


-- | Translate a Haskell string into a list of Template-Haskell declarations
stringToDecs :: String -> Q [Dec]
stringToDecs s = case parseDecs s of
    Left e -> error $ "Failed to parse module\n" ++ e
    Right decs -> return decs


-- | Given a string representing Haskell declarations, splice them and
-- into the haskell code, while also translating them into an Elm module
-- stored with the given varName
decsFromString :: String -> String -> DecsQ
decsFromString varName decString = decHaskAndElm varName (stringToDecs decString)

-- | Given a file containing Haskell declarations, splice them and
-- into the haskell code, while also translating them into an Elm module
-- stored with the given varName
decsFromFile :: String -> String -> DecsQ
decsFromFile varName filePath = do
  cd <- runIO getCurrentDirectory
  runIO $ putStrLn $ "CurrentDirectory " ++ cd
  decString <- runIO $ readFile filePath
  decHaskAndElm varName (stringToDecs decString)

baseCode =  "getType (Object d) = case (Dict.lookup \"__type\" d) of (Just (Json.String t)) -> t\n" 
               ++ "getCtor (Object d) = case (Dict.lookup \"__ctor\" d) of (Just (Json.String c)) -> c\n"
               ++ "varNamed (Object d) n= case (Dict.lookup (show n) d) of (Just val) -> val\n"
               ++ "mapJson f (Array l) = map f l\n"
               ++ "makeList  (Array l) = l\n"
               ++ "error s = case True of False -> s"
               ++ "\n\n--End Haskelm prelude\n"

moduleToString (Module [name] export imports elmDecs ) =
  let preamble = "module " ++ name ++ " where\nimport open Json\nimport Json\nimport Dict\n" ++ baseCode --TODO imports, exports
  in preamble ++ intercalate "\n" (map (show . Pretty.pretty) elmDecs)               
               
-- | Given haskell declarations wrapped in '[d| ... |]', splice them and
-- into the haskell code, while also translating them into an Elm module
-- stored with the given varName
decHaskAndElm :: String -> DecsQ -> DecsQ
decHaskAndElm varName dq = do
    decs <- dq
    --runIO $ putStrLn $ "Got pretty " ++ ( concat $ map (show . Pretty.pretty) $  HToE.toElm decs)
    elmString <- toElmString "Main" decs
    let elmExp = liftString elmString
    let pat = varP (mkName varName)
    let body = normalB elmExp
    --runIO $ putStrLn $ concatMap pprint decs
    elmDec <- valD pat body []
    --runIO $ putStrLn "****************************************\nStarting Elm Compilation"
    let modul = moduleFromString (TS.pack "Main") (TS.pack elmString )
    js <- runIO $ buildModules modul []
    --runIO $ putStrLn "****************************************\nEnding Elm Compilation"
    --runIO $ putStrLn $ "Generated js:\n" ++ js
    return $ decs ++ [elmDec]

