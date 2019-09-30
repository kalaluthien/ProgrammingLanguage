module Present ( Require(..), Cond(..), Gift, ID(..), shoppingList ) where

import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Debug.Trace

data Require = Req { getID :: ID, getCondList :: [Cond] }

data Cond = Items [Gift]
          | Same ID
          | Common Cond Cond
          | Except Cond [Gift]

type Gift = Int

data ID = A | B | C | D | E deriving (Eq, Ord, Enum, Show)

shoppingList :: [Require] -> [(ID, [Gift])]
shoppingList reqs = transit startPoint
  where startPoint = [(k, []) | k <- [A .. E]]
        condMap = [(k, v) | Req {getID = k, getCondList = v} <- reqs]
        transit prev = if prev == next then next else transit next
          where next = [(k, v) | k <- [A .. E], let v = go $ lookup k condMap]
                go Nothing = []
                go (Just conds) = sort . nub . concat $ query prev <$> conds

query :: [(ID, [Gift])] -> Cond -> [Gift]
query point = go
  where go (Items v) = v
        go (Same k) = concat . maybeToList $ lookup k point
        go (Common l r) = [y | x <- go l, y <-go r, x == y]
        go (Except c l) = [x | x <- go c, not $ elem x l]
