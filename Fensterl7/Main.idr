module Fensterl7.Main

import Data.List1
import Data.List

import Common.Input
import Fensterl6.Parser

-- Ex 1
median : List Nat -> Maybe Nat
median xs = let
  tot = length xs
  splitPoint = cast (cast tot / 2.0)
  sxs = splitAt splitPoint $ sort xs
  in case sxs of
          ([], [])     => Nothing
          (sx, [])     => last' sx
          (_ , (x::_)) => Just x

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
  res <- Input.readInput tokenizer grammar "Fensterl7/input"
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

