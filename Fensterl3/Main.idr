module Fensterl3.Main

import Data.List
import Data.List1

import Common.Input
import Fensterl3.Parser
import Fensterl3.Binaries

Predicate : Type
Predicate = Nat -> Nat -> Bool

countOs' : List Bit -> Nat
countOs' [] = 0
countOs' (x::xs) = let
    k = countOs' xs
  in case x of
    O => S k
    Z => k

countOs : List1 Bit -> Nat
countOs = countOs' . forget

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

criterium : Predicate -> List1 Bit -> Nat -> Bit
criterium p x n = if p (2 * countOs x) n then O else Z

discern : List1 (List Bit) -> (Nat, Nat)
discern input = let
    determine : Predicate -> List1 Bit -> Bit
    determine p x = criterium p x . length $ forget input
    proc : Predicate -> List1 (List Bit) -> Nat
    proc p = binToNat . toBin . map (determine p) . swap
  in (proc (>) input, proc (<) input)

-- Ex 2
o2crit : Predicate -> List1 Bit -> Bit
o2crit p x = criterium p x . length . forget $ x

filter' : List1 Bool -> List1 (List1 Bit) -> List1 (List1 Bit)
filter' ps xxs@(x:::_) = case fFilter xxs of
                  (x::xs) => x:::xs
                  _ => (x:::[])
                    where
                      fFilter : List1 (List1 Bit) -> List (List1 Bit)
                      fFilter = map snd . filter fst . forget . zip ps

retrieve' : Predicate -> Nat -> List1 (List1 Bit) -> List1 Bit
retrieve' _ _ (xs:::[]) = xs
retrieve' p n xxs = let
    sx = drop n . forget . swap $ xxs
  in case sx of
          (s::_) => let predicates = map (==o2crit p s) s
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

