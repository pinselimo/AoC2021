module Fensterl13.Main

import Data.List
import Data.List1

import Common.Input
import Fensterl13.Parser

-- Ex 1
ident : Ord a => (Direction, a) -> (a, a) -> Bool
ident (X, l) (x, _) = x > l
ident (Y, l) (_, y) = y > l

foldOne : (Abs a, Neg a) => a -> a -> a
foldOne by x = let
    d = abs $ x - by
  in by - d

fix : (Abs a, Neg a) => (Direction, a) -> (a, a) -> (a, a)
fix (X, l) = mapFst $ foldOne l
fix (Y, l) = mapSnd $ foldOne l

fold : (Abs a, Eq a, Neg a, Ord a) => (Direction, a) -> List (a, a) -> List (a, a)
fold instr paper = let
  nofold = filter (not . ident instr) paper
  folded = filter (ident instr) paper
  in nub $ nofold ++ map (fix instr) folded

main : HasIO io => io ()
main = do
  (paper, folds) <- Input.readInput tokenizer grammar "Fensterl13/input"
  printLn $ length $ fold (head folds) $ forget paper
