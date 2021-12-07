module Fensterl7.Main

import Data.List1
import Data.List

import AoC.Input
import Fensterl6.Parser

median : List Nat -> Maybe Nat
median xs = let
  tot = length xs
  splitPoint = cast (cast tot / 2.0)
  sxs = splitAt splitPoint $ sort xs
  in case sxs of
          ([], [])     => Nothing
          (sx, [])     => last' sx
          (_ , (x::_)) => Just x

diff : Int -> Int -> Int
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

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl7/input"
  printLn $ length $ forget res

  let m = median $ forget res
  printLn m
  case m of
       Nothing => printLn "Error"
       Just n  => do
         let fuelConsumption = map (diff (cast n) . cast) $ forget res
         --printLn fuelConsumption

         printLn $ sum fuelConsumption

  -- Ex 2
  let m = mean $ forget res
  printLn m
  case m of
       Nothing => printLn "Error"
       Just (n, n')  => do
         let fuelConsumption = map (toNonLFuel . diff (cast n) . cast) $ forget res
         let fuelConsumption' = map (toNonLFuel . diff (cast n') . cast) $ forget res

         printLn $ min (sum fuelConsumption) (sum fuelConsumption')

