module Main( main ) where

import System.Environment( getArgs )

import Language.Elm.TH
import Language.Haskell.TH




main = do
  (infile:_) <- getArgs
  source <- readFile infile
  result <- runQ $ do
    decs <- stringToDecs source
    toElmString "Main" decs  
  putStrLn result