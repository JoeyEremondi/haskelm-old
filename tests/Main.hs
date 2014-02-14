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


-- | Similarly, we can load a module from a file
elmString = $(translateToElm
    (defaultOptions {moduleName="Foo"})
    ("tests/files/module1.hs" ) )

-- | We can now access the elm strings we declared
main = do
  putStrLn "Generated elm strings:"
  mapM_ putStrLn [elmString]
  writeFile "src/Test.elm" elmString
  return ()
