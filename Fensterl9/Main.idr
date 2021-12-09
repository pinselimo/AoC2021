module Fensterl9.Main

import Data.Fin
import Data.List
import Data.List1

import AoC.Input
import Fensterl9.Parser
import Fensterl1.Comonad

data Store : s -> a -> Type where
  MkStore : (s -> a) -> s -> Store s a

Functor (Store s) where
  map f (MkStore g x) = MkStore (f . g) x

Comonad (Store s) where
  extract (MkStore f x) = f x
  duplicate (MkStore f x) = MkStore (\y => MkStore f y) x
  extend f s@(MkStore g x) = MkStore (f . MkStore g) x

pos : Store s a -> s
pos (MkStore _ idx) = idx

seek : s -> Store s a -> Store s a
seek idx (MkStore f _) = MkStore f idx

peek : s -> Store s a -> a
peek idx (MkStore f _) = f idx

Coord : Type
Coord = (Nat, Nat)

Grid : Type -> Type
Grid ty = Store Coord (Maybe ty)

infix 6 !!
(!!) : List a -> Nat -> Maybe a
l !! i = index' l <$> natToFin i (length l)

fromList : List (List a) -> Grid a
fromList ll = MkStore (\(x,y) => (ll !! x) >>= (!!y)) (0, 0)

toList : Nat -> Nat -> Grid a -> List (List a)
toList x_max y_max g = [mapMaybe ((`peek` g) . (x,)) [0..y_max] 
                       | x <- [0..x_max]
                       ]

fromList1 : List1 (List1 a) -> Grid a
fromList1 = fromList . map forget . forget

dimNeighbors : Nat -> List Nat
dimNeighbors Z = [Z, S Z]
dimNeighbors (S a) = [a..S (S a)]

neighbors : Coord -> List Coord
neighbors (x, y) = [(x',y')
  | x' <- dimNeighbors x
  , y' <- dimNeighbors y
  , x/=x' || y/=y'
  ]

isLowpoint : Ord a => Grid a -> Maybe Bool
isLowpoint g = let
    ns = mapMaybe (`peek` g) . neighbors . pos $ g
  in do
    x <- extract g
    pure $ all (x <) ns

scoreLows : (Ord a, Num a) => Grid a -> Maybe a
scoreLows g = do
    low <- isLowpoint g
    n <- extract g
    pure $ if low then n + 1 else 0

sumRow : Num a => Nat -> Nat -> Grid a -> a
sumRow x y g = case peek (x, y) g of
          Nothing => 0
          Just n  => n + sumRow x (S y) g

sumGrid' : Num a => Nat -> Grid a -> a
sumGrid' x g = case peek (x,0) g of
                   Nothing => 0
                   Just _  => sumRow x 0 g + sumGrid' (S x) g

sumGrid : Num a => Grid a -> a
sumGrid = sumGrid' 0

findLowpoints : (Num a, Ord a) => Grid a -> Grid a
findLowpoints = extend scoreLows

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl9/input"
  printLn res

  -- Ex 1
  printLn . sumGrid . findLowpoints . fromList1 $ res

