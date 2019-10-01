module Main ( main ) where

import Present

testcase :: [[Require]]
testcase = [ [ Req A [Items [1, 2], Common (Same B) (Same C)]
             , Req B [Common (Same C) (Items [2, 3])]
             , Req C [Items [1], Except (Same A) [3]]
             ]
           ]

result :: [[(ID, [Gift])]]
result = shoppingList <$> testcase

answer :: [[(ID, [Gift])]]
answer = [ [ (A, [1, 2])
           , (B, [2])
           , (C, [1, 2])
           , (D, [])
           , (E, [])
           ]
         ]

check :: [[(ID, [Gift])]] -> [[(ID, [Gift])]] -> [String]
check a b = go (prep a) (prep b)
  where prep = id
        go [] _ = []
        go _ [] = []
        go (x:xs) (y:ys) = pretty_print x y : go xs ys
        pretty_print x y =
          show (x == y) ++ ": res " ++ show x ++ " ans " ++ show y

main :: IO ()
main = putStr . unlines $ check result answer