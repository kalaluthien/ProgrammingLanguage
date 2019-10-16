module Main ( main ) where

import KminusParser

main :: IO ()
main = parseFile "./example.k-" >>= print
