module Main ( main ) where

import Control.Monad
import Data.Functor
import Data.List

import TreasureIsland

main :: IO ()
main = do
  putStr . unlines $ check result answer

check :: [[Key]] -> [[Key]] -> [String]
check a b = go (prep a) (prep b)
  where prep = id
        go [] _ = []
        go _ [] = []
        go (x:xs) (y:ys) = show (x == y) : go xs ys

testcase :: [Map]
testcase =
  let sBox = End StarBox
      nBox = End . NameBox
      gBox = ap Guide nBox
  in [ nBox "x"
     , gBox "x"
     , Branch (gBox "x") sBox
     , Branch (Guide "x" $ Branch (nBox "x") (nBox "x")) sBox
     , Branch (gBox "x") (Branch (gBox "y") sBox)
     , Branch (gBox "x") (gBox "y")
     , Branch (nBox "x") sBox
     , Guide "x" $ Branch (Branch (nBox "x") sBox) (gBox "y")
     , Branch (Branch (Branch (Guide "b" (Guide "a" $ Branch (nBox "a") (nBox "b"))) (nBox "c")) (gBox "d")) (nBox "e")
     , Branch (Branch (nBox "y") sBox) (Guide "x" (Branch (nBox "x") sBox))
     , Branch (nBox "z") (Guide "x" (Branch (Guide "y" (Branch (nBox "x") (nBox "y"))) sBox))
     , Branch (Guide "c"  (Branch (Branch (Guide "a" (Branch (nBox "a") (nBox "b"))) (Branch (nBox "b") (nBox "a"))) (nBox "c"))) sBox
     ]

answer :: [[Key]]
answer = fmap sort
         [ [Bar]
         , [Bar]
         , [Bar]
         , []
         , [Bar]
         , [Bar, Node Bar Bar]
         , [Bar, Node Bar Bar]
         , [Bar, Node Bar (Node (Node Bar Bar) Bar)]
         , [Bar, Node Bar Bar, Node (Node Bar Bar) (Node Bar Bar)]
         , [Bar, Node Bar Bar, Node Bar (Node (Node (Node Bar Bar) Bar) Bar)]
         , [Bar, Node Bar Bar, Node (Node (Node Bar Bar) Bar) Bar]
         , []
         ]

result :: [[Key]]
result = getReady <$> testcase
