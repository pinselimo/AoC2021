module Fensterl3.Main

import AoC.Input
import Fensterl3.Parser
import Fensterl3.Binaries

import Data.List
import Data.List1

Predicate : Type
Predicate = Nat -> Nat -> Bool

countOs : List Bit -> Nat
countOs [] = 0
countOs (x::xs) = case x of
  O => S k
  Z => k
  where 
    k : Nat
    k = countOs xs

interface Listlike (t : Type -> Type) where
  lcons : a -> t a -> t a

Listlike List where
  lcons a l = a::l

Listlike List1 where
  lcons a l1 = a:::(forget l1)

swap : (Functor t, Zippable t, Listlike t) => List1 (t Bit) -> t (List1 Bit)
swap = foldr1By (zipWith (\a => (lcons a))) (map (:::[]))

toBin : List Bit -> Bin
toBin = toBin' . reverse
  where
    toBin' : List Bit -> Bin
    toBin' [] = Z
    toBin' (O::r) = B1 $ toBin' r
    toBin' (Z::r) = B0 $ toBin' r

discern : List1 (List Bit) -> (Nat, Nat)
discern input = let
    determine : Predicate -> Nat -> Bit
    determine p x = if p (2*x) (length $ forget input) then O else Z
    proc : Predicate -> List1 (List Bit) -> Nat
    proc p = binToNat . toBin . map (determine p . countOs . forget) . swap
  in (proc (>) input, proc (<) input)

-- Ex 2
o2crit : Predicate -> List Bit -> Bit
o2crit p x = if p (2 * countOs x) (length x) then O else Z

filter' : List1 Bool -> List1 (List1 Bit) -> List1 (List1 Bit)
filter' ps xxs@(x:::_) = case map snd . filter fst . forget . zip ps $ xxs of
                    (x::xs) => x:::xs
                    _ => (x:::[])

retrieve' : Predicate -> Nat -> List1 (List1 Bit) -> List1 Bit
retrieve' _ _ (xs:::[]) = xs
retrieve' p n xxs = let
    sx : List (List1 Bit)
    sx = drop n . forget . swap $ xxs
  in case sx of
          (s::_) => let predicates = map (== (o2crit p $ forget s)) s
                    in retrieve' p (S n) $ filter' predicates xxs
          _ => head xxs

retrieve : Predicate -> Nat -> List1 (List1 Bit) -> Nat
retrieve p n = binToNat . toBin . forget . retrieve' p n

discern' : List1 (List1 Bit) -> (Nat, Nat)
discern' xs = let
  sx  = forget $ swap xs
  in (retrieve (>=) 0 xs, retrieve (<) 0 xs)
 
main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl3/input"
  printLn . length . forget $ res

  printLn . uncurry (*) . discern . map forget $ res

  printLn . uncurry (*) . discern' $ res

