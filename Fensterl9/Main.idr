module Fensterl9.Main

import Data.Either
import Data.Fin
import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap

import Common.Input
import Common.Comonad
import Common.Comonad.Store
import Fensterl9.Parser

Coord : Type
Coord = (Nat, Nat)

Grid : Type -> Type
Grid ty = Store Coord (Maybe ty)

infix 6 !!
(!!) : List a -> Nat -> Maybe a
l !! i = index' l <$> natToFin i (length l)

fromList : List (List a) -> Grid a
fromList ll = MkStore (\(x,y) => (ll !! x) >>= (!!y)) (0, 0)

toRow : Nat -> Nat -> Grid a -> List a
toRow x y g = case peek (x, y) g of
          Nothing => []
          Just n  => n :: toRow x (S y) g

toList' : Nat -> Nat -> Grid a -> List (List a)
toList' x y g = case peek (x, y) g of
                   Nothing => []
                   Just _  => toRow x y g :: toList' (S x) y g

toLists : Grid a -> List (List a)
toLists = toList' 0 0

dimNeighbors : Nat -> List Nat
dimNeighbors (S a) = [a..a+2]
dimNeighbors _ = [0, 1]

neighbors : Coord -> List Coord
neighbors (x, y) = [(x',y')
  | x' <- dimNeighbors x
  , y' <- dimNeighbors y
  , x/=x' || y/=y'
  ]

isLowpoint : Ord a => Grid a -> Maybe Bool
isLowpoint g = let
    ns = mapMaybe (`peek` g) . neighbors . pos $ g
  in pure . (`all` ns) . (<) =<< extract g 

scoreLows : (Ord a, Num a) => Grid a -> Maybe a
scoreLows g = do
    low <- isLowpoint g
    n <- extract g
    pure $ if low then n + 1 else 0

findLowpoints : (Num a, Ord a) => Grid a -> Grid a
findLowpoints = extend scoreLows

-- Ex 2
isBasin : (Num a, Ord a) => Grid a -> Maybe Bool
isBasin g = extract g >>= pure . (<9)

basinCoords : Grid Bool -> Maybe (Either () Coord)
basinCoords g = extract g >>= pure . (`ifThenElse` (Right $ pos g) $ Left ())

neighborOf : Coord -> Coord -> Bool
neighborOf (x,y) (x', y') = let
  ym = y == y'
  xm = x == x'
  in (xm || ym) && (x == S x' || y == S y' || x' == S x || y' == S y)

combineNeighbors : Coord -> List (List Coord) -> List (List Coord)
combineNeighbors x l = case partition (any (neighborOf x)) l of
                         (z::zs, ys) => (x::z) :: zs ++ ys
                         ([]   ,  _) => pure x :: l

combineBasins : List Coord -> List (List Coord) -> List (List Coord)
combineBasins xs xxs = case partition (any ((`any` xs) . neighborOf)) xxs of
         ([],    _) => xs :: xxs
         (ns, xxs') => combineBasins (xs++concat ns) xxs'

combine : List Coord -> List (List Coord)
combine = foldr combineBasins [] . foldr combineNeighbors []

ex2Res : List (List Coord) -> Nat
ex2Res = foldr (*) 1 . take 3 . reverse . sort . map length

getEx2Res : Grid (Either () Coord) -> Nat
getEx2Res = ex2Res . combine . mapMaybe getRight . concat . toLists

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl9/input"
  --printLn res

  let grid = fromList . map forget . forget $ res

  -- Ex 1
  printLn . sum . map sum . toLists . findLowpoints $ grid

  -- Ex 2
  printLn . getEx2Res . extend basinCoords . extend isBasin $ grid

