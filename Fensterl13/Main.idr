module Fensterl13.Main

import Data.String
import Data.List
import Data.List1
import Data.SortedSet

import Common.Input
import Fensterl13.Parser

setMap : (Ord a, Ord b) => (a -> b) -> SortedSet a -> SortedSet b
setMap f = foldr (\x, s => insert (f x) s) empty

filter : Ord a => (a -> Bool) -> SortedSet a -> SortedSet a
filter p = foldr (\x, s => if p x then insert x s else s) empty

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

fold : (Abs a, Eq a, Neg a, Ord a) => (Direction, a) -> SortedSet (a, a) -> SortedSet (a, a)
fold instr paper = let
  nofold = filter (not . ident instr) paper
  folded = filter (ident instr) paper
  in nofold `union` setMap (fix instr) folded


foldFirst : List1 (Direction, Integer) -> List1 (Integer, Integer) -> Nat
foldFirst fs = length . Data.SortedSet.toList . fold (head fs) . fromList . forget

-- Ex 2
show' : (Num a, Ord a, Range a) => SortedSet (a, a) -> String
show' l = unlines . map (foldr (++) "")
        $ do 
          y <- [0..max' snd]
          pure $ do
                 x <- [0..max' fst]
                 pure $ if contains (x,y) l then "\x1B[36m█\x1B[37m" else " "
  where
    max' : ((a, a) -> a) -> a
    max' f = foldr max 0 . setMap f $ l

foldAll : List1 (Direction, Integer) -> List1 (Integer, Integer) -> String
foldAll fs start = show' . foldl (flip fold) (fromList $ forget start) $ fs

main : HasIO io => io ()
main = do
  (paper, folds) <- Input.readInput tokenizer grammar "Fensterl13/input"

  -- Ex 1
  printLn $ foldFirst folds paper

  -- Ex 2
  putStrLn $ foldAll folds paper

