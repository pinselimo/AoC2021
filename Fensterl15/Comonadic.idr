module Fensterl15.Comonadic

import Data.Fin
import Data.Maybe
import Data.List
import Data.List1

import Common.Input
import Common.Comonad
import Common.Comonad.Store
import Fensterl11.Parser

Coord : Type
Coord = (Integer, Integer)

Grid : Type -> Type
Grid ty = Store Coord (Maybe ty)

infix 6 !!
(!!) : List a -> Integer -> Maybe a
l !! i = index' l <$> integerToFin i (length l)

infixl 1 &>=
(&>=) : (Functor f, Monad m) => f (m a) -> (a -> m b) -> f (m b)
m &>= f = (>>= f) <$> m

fromList : List (List a) -> Grid a
fromList ll = MkStore (\(x,y) => (ll !! x) >>= (!!y)) (0, 0)

neighbors : Coord -> List Coord
neighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Ex 1
lowest : (Ord a, Num a) => List (a, Coord) -> a -> Maybe (a, Coord)
lowest [] _ = Nothing
lowest (x::xs) y = Just
                 $ mapFst (+y)
                 $ foldr1 (\a, b => if fst a < fst b then a else b) (x:::xs)

findLowest : (Ord a, Num a) => List Coord -> Grid a -> Maybe (a, Coord)
findLowest visited g = let
  cp = pos g
  ns = neighbors cp
  in if (0,0) `elem` ns then (, (0,0)) <$> extract g
  else let
  nc = filter (not . (`elem` visited)) $ mapMaybe (\n => const n <$> peek n g) $ ns
  nw = mapMaybe (\c => mapSnd (const c)
                   <$> (findLowest (cp::visited) $ seek c g)) nc
  in lowest nw =<< extract g

extractPath : Grid (a, Coord) -> List (a, Coord)
extractPath g = case extract g of
                     Nothing => []
                     Just r@(_, src) => if pos g == (0,0)
                                           then []
                                           else r :: (extractPath $ seek src g)

problem1 : (Num a, Ord a) => List1 (List1 a) -> List (a, Coord)
problem1 ls = let
  g = fromList $ forget $ map forget ls
  dim = natToInteger (length $ forget ls)-1
  in extractPath $ seek (dim, dim) $ extend (findLowest []) g


main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl15/testinput"
  printLn $ problem1 $ res

