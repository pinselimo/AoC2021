module Fensterl5.Main

import Data.List1
import Data.List

import AoC.Input
import Fensterl5.Parser

data Dir : Type where
  Horizontal : Dir
  Vertical : Dir

isHorV : Eq n => Vent n -> Maybe Dir
isHorV (MkVent (x1, y1) (x2, y2)) = if x1 == x2 
                                       then Just Horizontal
                                       else if y1 == y2 
                                       then Just Vertical
                                       else Nothing

ventCoverage : (Range n, Eq n) => Vent n -> List (n, n)
ventCoverage v@(MkVent (x1, y1) (x2, y2)) = 
  let 
    dir = isHorV v
  in case dir of
          Nothing => []
          Just Horizontal => (x1,) <$> [y1..y2]
          Just Vertical => (,y1) <$> [x1..x2]

fullCoordinates : (Range n, Eq n) => List1 (Vent n) -> List (n, n)
fullCoordinates = concat . map ventCoverage

nDangerous : (Range n, Eq n, Ord n) => List (n, n) -> Nat
nDangerous = length . filter (>=2) . map (length . forget) . group . sort

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl5/input"

  printLn. nDangerous. fullCoordinates $ res

