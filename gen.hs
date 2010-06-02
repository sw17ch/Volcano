module Main where

import Language.Haskell.Exts
import Text.Show.Pretty
import Data.Generics.Uniplate

main :: IO ()
main = do
    spec <- parseFile "spec.hs"
    putStrLn $ ppShow spec
