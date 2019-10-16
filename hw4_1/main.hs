module Main ( main ) where

import Kminus

main :: IO ()
main = do
  program <- parseFile "./example.k-"
  print program
