module Main( main ) where

import System.Environment( getArgs )

import Language.Elm.TH
import Language.Haskell.TH




main = do
  (infile:_) <- getArgs
  source <- readFile infile
  result <- runQ $ do
    let decs = decsFromString source
    let options = Options True False [] "Main" ""
    LitE (StringL str) <- elmStringExp options decs
    return str
  putStrLn result