module Fensterl07.Main

import Data.List1
import Data.List
import Data.Nat

import Common.Input
import Fensterl06.Parser

-- Ex 1
median : Ord a => List a -> Maybe a
median xs = let
  half = div (length xs) 2
  in case drop half $ sort xs of
          (x::_) => Just x
          _ => Nothing

diff : (Abs a, Neg a) => a -> a -> a
diff x y = abs (x - y)

-- Ex 2
bounds : Double -> (Nat, Nat)
bounds d = let
  n = cast d
  in (n, S n)

mean : List Nat -> Maybe (Nat, Nat)
mean [] = Nothing
mean xs = Just $ bounds $ (cast $ sum xs) / (cast $ length xs)

toNonLFuel : (Num a, Range a) => a -> a
toNonLFuel n = sum [0..n]

-- Fuel calc
totalFuel : (Num a, Abs a, Neg a) => (a -> a) -> a -> List1 a -> a
totalFuel f n = sum . map (f . diff n) . forget

fuel1 : Int -> List1 Int -> Int
fuel1 = totalFuel id

fuel2 : Int -> List1 Int -> Int
fuel2 = totalFuel toNonLFuel

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl07/input"
  let res' = map cast res
  printLn $ length $ forget res

  let m = median $ forget res
  printLn m
  case m of
       Nothing => printLn "Error"
       Just n  => do
         printLn $ fuel1 (cast n) res'

  -- Ex 2
  let m = mean $ forget res
  printLn m
  case m of
       Nothing => printLn "Error"
       Just (n, n')  => do
         printLn $ min (fuel2 (cast n) res') (fuel2 (cast n') res') 

