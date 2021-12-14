module Fensterl14.Main

import Data.List
import Data.List1
import Data.Maybe
import Data.SortedMap

import Common.Input
import Fensterl14.Parser

pairsZip : List a -> List (a, a)
pairsZip xs = zip xs (drop 1 xs)

toMap : (Ord s, Eq s) => List (s,s) -> SortedMap (s,s) Nat
toMap l = fromList $ map (\p => (p, count (==p) l)) l

substitute1 : (Eq s, Ord s) => SortedMap (s,s) s -> ((s,s), Nat)
                            -> SortedMap (s,s) Nat
substitute1 m (p@(x,y), n) = map (*n) . toMap
                           $ fromMaybe [] [[(x,z), (z,y)] | z <- lookup p m]

substituteOnce : (Eq s, Ord s) => SortedMap (s,s) s -> SortedMap (s,s) Nat
                               -> SortedMap (s,s) Nat
substituteOnce m = foldr (mergeWith (+)) empty . map (substitute1 m) . toList

substituteN : (Eq s, Ord s) => Nat -> SortedMap (s, s) s -> SortedMap (s, s) Nat
                            -> SortedMap (s, s) Nat
substituteN (S k) m = substituteN k m . substituteOnce m
substituteN 0 _ = id

occurences : Ord s => s -> SortedMap (s, s) Nat -> SortedMap s Nat
occurences h m = let
  first = fromList $ pure (h, 1)
  m' = map (\(k, v) => fromList . pure $ (snd k, v)) $ SortedMap.toList m
  in foldr (mergeWith (+)) first m'

problem : Ord s => s -> SortedMap (s,s) Nat -> Integer
problem h m = let
  vs = map natToInteger $ occurences h m
  ma = foldr max 0 vs
  in ma - foldr min ma vs

main : HasIO io => io ()
main = do
  (poly, rules) <- Input.readInput tokenizer grammar "Fensterl14/input"
  let m = toMap . pairsZip . forget $ poly
  printLn . problem (head poly) . substituteN 10 rules $ m
  printLn . problem (head poly) . substituteN 40 rules $ m


-- Proofs
pairs : List a -> List (a,a)
pairs ([]) = []
pairs (x::[]) = []
pairs (x::y::xs) = (x,y)::pairs (y::xs)

lemmaPairs : (l: List a) -> pairs l = pairsZip l
lemmaPairs [] = Refl
lemmaPairs (x :: []) = Refl
lemmaPairs (x :: (y :: xs)) = cong ((x,y) ::) (lemmaPairs (y::xs))

