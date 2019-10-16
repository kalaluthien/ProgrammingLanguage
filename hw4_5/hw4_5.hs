module Main ( main ) where

import Present

main :: IO ()
main = putStr . unlines $ check result answer

check :: [[(ID, [Gift])]] -> [[(ID, [Gift])]] -> [String]
check a b = go (prep a) (prep b)
  where prep = id
        go [] _ = []
        go _ [] = []
        go (x:xs) (y:ys) = show (x == y) : go xs ys

testcase :: [[Require]]
testcase = [ [ Req A [Items [1, 2], Common (Same B) (Same C)]
             , Req B [Common (Same C) (Items [2, 3])]
             , Req C [Items [1], Except (Same A) [3]]
             , Req D []
             , Req E [Same D]
             ]
           , [ Req A [Same B, Same C]
             , Req B [Same A, Same C]
             , Req C [Same A, Same B]
             , Req D [Except (Same E) [1]]
             , Req E [Except (Same D) [2]]
             ]
           , [ Req A [ Items [1..3]
                     , Except (Items [5..7]) [6]
                     , Common (Same D) (Same E) ]
             , Req B [ Common (Same A) (Same B)
                     , Common (Same B) (Same C)
                     , Except (Same D) [9] ]
             , Req C [ Common (Same B) (Same C)
                     , Except (Same E) [1, 6]
                     , Common (Same A) (Same D) ]
             , Req D [Items [4..10], Same B, Same C]
             , Req E [ Except (Same A) [3]
                     , Items [9..11]
                     , Common (Common (Same B) (Same D)) (Items [1..7]) ]
             ]
           , [ Req A [Items [1, 2, 3, 1, 2, 3]]
             , Req B [Same A]
             , Req C [Same A, Items [1, 2]]
             , Req D [Same A, Items [4]]
             , Req E [Same D] ]
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
         , [ (A, [])
           , (B, [])
           , (C, [])
           , (D, [])
           , (E, [])
           ]
         , [ (A, [1, 2, 3, 4, 5, 6, 7, 9, 10, 11])
           , (B, [2, 4, 5, 6, 7, 8, 10, 11])
           , (C, [2, 4, 5, 6, 7, 9, 10, 11])
           , (D, [2, 4, 5, 6, 7, 8, 9, 10, 11])
           , (E, [1, 2, 4, 5, 6, 7, 9, 10, 11])
           ]
         , [ (A, [1, 2, 3])
           , (B, [1, 2, 3])
           , (C, [1, 2, 3])
           , (D, [1, 2, 3, 4])
           , (E, [1, 2, 3, 4])
           ]
         ]
