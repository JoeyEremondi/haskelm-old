{-|
Haskelm test suite
For the moment, just contains some basic tests
This file also serves as an example of how to 
translate Elm from different sources
-}

{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiWayIf #-}

import Language.Elm.TH
import Data.List (intercalate)
import Control.Monad

-- We can get the string for the Elm source of a translation
-- using ElmStringExp
-- We use decsFromString to convert the string into a Haskell expression
elmString1 = $(elmStringExp defaultOptions $ decsFromString $ intercalate "\n" ["x = 3",
                                                                  "y = 4",
                                                                  "fun x = x + 1"])
                                                                  
-- | If we want to include the Haskell declarations as well as the elm String,
-- we use declareTranslation
-- This module will contain a Haskell variable named "elmString2"
-- As well as the decs for Local1 and Local2
-- The templateHaskell declaration brackets [d| |] mean we don't need to use decsFromString
$(declareTranslation 
  (Options {makeJson = True,
            declareHaskell=True,
            elmImports = [],
            moduleName="Main",
            varName="elmString2" }) 
  [d| data Local1 = Local1 Int
      data Local2 = Local2 String
  |])

-- | Similarly, we can load a module from a file
$(declareTranslation
    (defaultOptions {moduleName="Foo", varName="elmString3"})
    (decsFromModuleFile "tests/files/module1.hs" ))
  
-- |We can now get at our declared Haskell code
accessDecs (Local1 x) = Local2 (show x)

-- | We can now access the elm strings we declared
main = do
  putStrLn "Generated elm strings:"
  mapM_ putStrLn [elmString1, elmString2, elmString3]
  return ()
