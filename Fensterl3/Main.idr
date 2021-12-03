module Fensterl3.Main

import AoC.Input
import Fensterl3.Parser
import Fensterl3.Binaries

import Data.List
import Data.List1

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
  determine : (Nat -> Nat -> Bool) -> Nat -> Bit
  determine f x = if f (2*x) (length $ forget input) then O else Z
  proc : (Nat -> Nat -> Bool) -> List1 (List Bit) -> Nat
  proc f = binToNat . toBin . map (determine f . countOs) . swap
  in (proc (>) input, proc (<) input)

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl3/input"
  printLn . length . forget $ res

  printLn . uncurry (*) . discern $ res

