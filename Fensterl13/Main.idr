module Fensterl13.Main

import Data.String
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


foldFirst : List1 (Direction, Integer) -> List1 (Integer, Integer) -> Nat
foldFirst fs = length . fold (head fs) . forget

-- Ex 2
show' : (Num a, Ord a, Range a) => List (a, a) -> String
show' l = unlines . map (foldr (++) "")
        $ do 
          y <- [0..max' snd]
          pure $ do
                 x <- [0..max' fst]
                 pure $ if (x,y) `elem` l then "\x1B[36m█\x1B[37m" else " "
  where
    max' : ((a, a) -> a) -> a
    max' f = foldr max 0 . map f $ l

foldAll : List1 (Direction, Integer) -> List1 (Integer, Integer) -> String
foldAll fs start = show' . foldl (flip fold) (forget start) $ fs

main : HasIO io => io ()
main = do
  (paper, folds) <- Input.readInput tokenizer grammar "Fensterl13/input"

  -- Ex 1
  printLn $ foldFirst folds paper

  -- Ex 2
  putStrLn $ foldAll folds paper

