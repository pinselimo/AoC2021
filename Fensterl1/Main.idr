module Fensterl1.Main

import Data.List
import Data.List1

import Common.Input
import Fensterl1.Comonad
import Fensterl1.Parser

-- For chaining many printLns
%ambiguity_depth 6

-- First exercise

deeper : Ord a => a -> a -> Bool
deeper = (<)

countDeeperIdiomatic : Ord a => List a -> Nat
countDeeperIdiomatic (x::y::rest) = let
    next = countDeeperIdiomatic (y::rest)
  in if deeper x y then S next else next
countDeeperIdiomatic _ = Z

-- Point free example added for funsies
countDeeperPointfree : Ord a => List1 a -> Nat
countDeeperPointfree xs@(_:::xs') = length . map fst . filter (uncurry deeper) . zip (forget xs) $ xs'

countDeeperPointfree' : Ord a => List a -> Nat
countDeeperPointfree' xs@(_::xs') = length . map fst . filter (uncurry deeper) . zip xs $ xs'
countDeeperPointfree' _ = Z

-- Second exercise

pairThreeIdiomatic : Num a => List a -> List a
pairThreeIdiomatic (x::y::z::rest) = (x + y + z) :: pairThreeIdiomatic (y::z::rest)
pairThreeIdiomatic _ = []

-- Point free example added for funsies
pairThreePointfree : Num a => List a -> List a
pairThreePointfree = map (sum . take 3) . filter ((>2) . length) . map (uncurry drop) . prep
  where prep : List a -> List (Nat, List a)
        prep l = zip (rangeFromTo 0 (length l)) $ replicate (length l) l

-- Comonad
Comonad List1 where
  extract (a ::: _) = a
  duplicate w@(a ::: aas) = w ::: (case aas of
                               [] => []
                               (a'::as) => forget $ duplicate (a':::as))

-- extend : Comonad w => (w a -> b) -> w a -> w b
-- extend f w = map f $ duplicate w

deeper' : Ord a => List1 a -> Bool
deeper' (a:::aas) = case aas of
                        (b::as) => deeper a b
                        [] => False

countDeeperComonad : Ord a => List1 a -> Nat
countDeeperComonad = length . filter id . forget . extend deeper'

sumOf3 : Num a => List1 a -> a
sumOf3 (x:::xs) = case xs of
                       (y::z::_) => x + y + z
                       _ => 0

pairThreeComonad : Num a => List1 a -> List1 a
pairThreeComonad = extend sumOf3


main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl1/input"
  printLn $ length $ forget res

  let r1 = countDeeperIdiomatic $ forget res
  printLn r1

  let r1' = countDeeperPointfree res
  printLn r1'

  let r1'' = countDeeperComonad res
  printLn r1''

  let r2 = countDeeperIdiomatic $ pairThreeIdiomatic $ forget res
  printLn r2

  let r2' = countDeeperIdiomatic $ pairThreePointfree $ forget res
  printLn r2'

  let r2'' = countDeeperComonad $ pairThreeComonad res
  printLn r2''

