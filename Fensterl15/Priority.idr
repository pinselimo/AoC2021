module Fensterl15.Main

import Data.List
import Data.List1
import Data.Maybe
import Data.SortedMap

import Common.Input
import Fensterl15.Parser

Coord : Type
Coord = (Integer, Integer)

neighbors : Coord -> List Coord
neighbors (x, y) = filter (\(x,y) => x >= 0 && y >= 0) $ [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

matrix : List1 (List1 a) -> SortedMap Coord (a, Coord)
matrix l@(e:::_) = let
  xs = map natToInteger [0..length $ forget l]
  ys = map natToInteger [0..length $ forget e]
  cls = concat . zipWith (\x => zipWith (\y => ((x,y),) . (,(x,y))) ys . forget) xs . forget $ l
  in fromList cls

-- Ex 1
WeightStack : Type -> Type
WeightStack a = SortedMap (a, Integer) (List (List1 Coord))

app : k -> v -> SortedMap k (List v) -> SortedMap k (List v)
app k v m = insert k (v::(fromMaybe [] $ lookup k m)) m

nbrs : List Coord -> SortedMap Coord (a, Coord) -> Coord -> List (a, Coord)
nbrs v m = mapMaybe (`lookup` m) . filter (not . (`elem` v)) . neighbors

length' : List1 a -> Integer
length' = (-1*) . natToInteger . length . forget

rate : (Num a, Neg a) => (a,a) -> a
rate (x,y) = - (x*y)

initS : (Ord a, Num a) => Coord -> WeightStack a
initS cs = fromList [((0,0), [cs:::[]])]

path : (Eq a, Ord a, Num a, Neg a) => SortedMap Coord (a, Coord) -> WeightStack a -> WeightStack a
path m s = let
  next = leftMost s
  in case next of
          Nothing => s
          Just ((w,l), xs) => foldr (\p, ac => foldr (\(w',c), ac' => app (w+w', rate c) (c:::forget p) ac') ac $ nbrs (forget p) m $ head p) (delete (w,l) s) xs

calc : (Eq a, Ord a, Num a, Neg a) => Coord -> SortedMap Coord (a, Coord) -> WeightStack a -> (a, List1 Coord)
calc stop m s = let
  s' = path m s
  in case leftMost s' of
          Nothing => calc stop m s'
          Just ((w,l), ps) => case filter ((stop==) . head) ps of
                               [] => calc stop m s'
                               (x::_) => (w, x)

calcN : (Eq a, Ord a, Num a, Neg a) => Integer -> Coord -> SortedMap Coord (a, Coord) -> WeightStack a -> WeightStack a-- (a, List1 Coord)
calcN k stop m s = let
  s' = path m s
  in case leftMost s' of
          Nothing => calcN (k-1) stop m s'
          Just ((w,l), ps) => case filter (\p => k <= 0 || stop == head p) ps of
                               [] => calcN (k-1) stop m s'
                               (x::_) => s' -- (w, x)

proc : (Eq a, Ord a, Num a, Neg a) => List1 (List1 a) -> (a, List1 Coord)
proc ls = let
  m = matrix ls
  xdim = natToInteger (length $ forget ls) - 1
  ydim = natToInteger (length $ forget $ head ls) - 1
  in calc (xdim, ydim) m (initS (0,0))

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl15/input"
  -- printLn $ path (matrix res) (initS (0,0))
  --printLn $ take 1 $ SortedMap.toList $ proc $ map (map natToInteger) $ res
  printLn $ proc $ map (map natToInteger) $ res

