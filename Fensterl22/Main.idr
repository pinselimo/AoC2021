module Fensterl22.Main

import Data.List1

import Common.Input
import Common.Comonad
import Common.Comonad.Store
import Fensterl22.Parser

Coord : Type
Coord = (Int, Int, Int)

Grid : Type
Grid = Store Coord State

init : Grid
init = MkStore (const Off) (0, 0, 0)

step : (State, (Command, Command, Command)) -> Grid -> State
step (st, (_, (x1, x2)), (_, (y1, y2)), (_, (z1, z2))) g = let
  (x, y, z) = pos g
  in if   (x1 <= x && x <= x2)
       && (y1 <= y && y <= y2)
       && (z1 <= z && z <= z2)
     then st
     else extract g

proc : Foldable t => t (State, (Command, Command, Command)) -> Grid
proc = foldl (\g => (`extend` g) . step) init

result : Grid -> Nat
result g = sum [ (if peek (x,y,z) g == On then 1 else 0)
               | x <- [-50..50]
               , y <- [-50..50]
               , z <- [-50..50]
               ]

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl22/input"
  printLn . result . proc $ res

