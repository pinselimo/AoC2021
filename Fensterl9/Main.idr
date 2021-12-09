module Fensterl9.Main

import Data.Either
import Data.Fin
import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap

import Common.Input
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

findLowpoints : (Num a, Ord a) => Grid a -> Grid a
findLowpoints = extend scoreLows

-- Ex 2
isBasin : (Num a, Ord a) => Grid a -> Maybe Bool
isBasin g = extract g >>= pure . (<9)

basinCoords : Grid Bool -> Maybe (Either () Coord)
basinCoords g = extract g >>= \p => pure $ if p then (Right $ pos g) else Left ()

infixr 6 ?#
(?#) : Coord -> Coord -> Bool
(x,y) ?# (x', y') = (x == x' && y == S y' || y == y' && x == S x')
                 ||
                    (x == x' && y' == S y || y == y' && x' == S x)

combineNeighbors : List (List Coord) -> List Coord -> List (List Coord)
combineNeighbors l [] = l
combineNeighbors l (x::xs) = case partition (any (x?#)) l of
                        ([], _) => combineNeighbors ([x]::l) xs
                        ((z::zs, ys)) => combineNeighbors ((x::z)::zs++ys) xs

combineBasins : List (List Coord) -> List (List Coord)
combineBasins [] = []
combineBasins (x::xs) = case partition (any (\n => any (n?#) x)) xs of
                       (ns, xs') => (x ++ concat ns) :: combineBasins xs'

combineUntilFinished : List (List Coord) -> List (List Coord)
combineUntilFinished xs = let
  xs' = combineBasins xs
  in if xs == xs' then xs else combineUntilFinished xs'

combine : List Coord -> List (List Coord)
combine = combineUntilFinished . combineNeighbors []

ex2Res : List (List Coord) -> Nat
ex2Res = foldr (*) 1 . take 3 . reverse . sort . map length

getEx2Res : Grid (Either () Coord) -> Nat
getEx2Res = ex2Res . combine . mapMaybe getRight . concat . toLists

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl9/input"
  --printLn res

  let grid = fromList1 res

  -- Ex 1
  printLn . sum . map sum . toLists . findLowpoints $ grid

  -- Ex 2
  printLn . getEx2Res . extend basinCoords . extend isBasin $ grid

