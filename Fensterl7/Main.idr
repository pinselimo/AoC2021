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

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl7/input"
  --printLn res

  let m = median $ forget res
  printLn m
  case m of
       Nothing => printLn "Error"
       Just n  => do
         let fuelConsumption = map (diff (cast n) . cast) $ forget res
         --printLn fuelConsumption

         printLn $ sum fuelConsumption


