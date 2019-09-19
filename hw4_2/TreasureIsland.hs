module TreasureIsland ( Treasure(..), Key(..), Map(..), getReady ) where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.List

data Treasure = StarBox | NameBox String deriving Eq

data Key = Bar | Node Key Key deriving Eq

instance Ord Key where
  compare Bar Bar = EQ
  compare Bar _ = LT
  compare _ Bar = GT
  compare (Node k1 k2) (Node k3 k4) = compare k1 k3 <> compare k2 k4

data Map = End Treasure | Branch Map Map | Guide String Map deriving Eq

data TypeTerm = Nil | Tip String | Bin TypeTerm TypeTerm deriving Eq

type TypeModel = TypeTerm -> TypeTerm

type TypeEnv = MaybeT (State [(String, TypeTerm)])

allocEnv :: TypeEnv TypeTerm
allocEnv = do
  env <- get
  let x = '#' : show (length env)
  modify (\env -> (x, Tip x) : env)
  return (Tip x)

addEnv :: String -> TypeTerm -> TypeEnv ()
addEnv x t = modify ((x, t) :)

substEnv :: TypeModel -> TypeEnv TypeModel
substEnv s = (modify . fmap . second) s >> return s

getReady :: Map -> [Key]
getReady m = sort . nub . typeCheck $ maybeSubst
  where (x0, v0, e0) = ("#0", Tip x0, [(x0, v0)])
        (maybeSubst, symtab) = runState (runMaybeT $ solveM m v0) e0
        typeCheck :: Maybe TypeModel -> [Key]
        typeCheck Nothing = []
        typeCheck (Just subst) = translate subst symtab m

translate :: TypeModel -> [(String, TypeTerm)] -> Map -> [Key]
translate subst symtab = go
  where go (End StarBox) = [Bar]
        go (End (NameBox x)) = [fromTerm . subst . query $ x]
        go (Guide _ e) = go e
        go (Branch e e') = go e ++ go e'
        fromTerm :: TypeTerm -> Key
        fromTerm Nil = Bar
        fromTerm (Tip x) = fromTerm . derive . subst $ (Tip x)
        fromTerm (Bin l r) = Node (fromTerm l) (fromTerm r)
        derive :: TypeTerm -> TypeTerm
        derive (Bin l r) = Bin (derive . subst $ l) (derive . subst $ r)
        derive _ = Nil
        query :: String -> TypeTerm
        query x = fromJust $ lookup x symtab

solveM :: Map -> TypeTerm -> TypeEnv TypeModel
solveM (End StarBox) t = do
  s <- unify Nil t
  substEnv s

solveM (End (NameBox x)) t = do
  env <- get
  case lookup x env of
    Nothing -> do
      v <- allocEnv
      addEnv x v
      s <- unify v t
      substEnv s
    Just v -> do
      s <- unify v t
      substEnv s

solveM (Guide x e) t = do
  v <- allocEnv
  v' <- allocEnv
  s <- unify t (Bin v v')
  addEnv x (s v)
  s' <- solveM e (s v')
  substEnv (s' . s)

solveM (Branch e e') t = do
  v <- allocEnv
  s <- solveM e (Bin v t)
  s' <- solveM e' (s v)
  substEnv (s' . s)

unify :: TypeTerm -> TypeTerm -> TypeEnv TypeModel
unify (Tip x) t | x `notOccurIn` t =
  return $ (Tip x) `to` t
    where a `to` b = \x -> if a == x then b else x
          notOccurIn :: String -> TypeTerm -> Bool
          notOccurIn x = go
            where go Nil = True
                  go (Tip x') = x /= x'
                  go (Bin l r) = go l && go r

unify t (Tip x) = unify (Tip x) t

unify (Bin l l') (Bin r r') = do
  s <- unify l r
  s' <- unify (s l') (s r')
  return (s' . s)

unify t t' = do
  guard (t == t')
  return id
