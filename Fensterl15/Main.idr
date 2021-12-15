module Fensterl15.Main
-- Let's just try Dijkstra
import Data.List1
import Data.List
import Data.List.Extra
import Data.SortedMap
import Data.SortedSet
import Data.Maybe
import Control.Monad.State

import Common.Input
import Fensterl11.Parser

Coord : Type
Coord = (Int, Int)

neighbors : Coord -> List Coord
neighbors (x, y) = filter (\(x,y) => x >= 0 && y >= 0) $ [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

Distances : Type
Distances = SortedMap Coord Nat

Preds : Type
Preds = SortedMap Coord Coord

Queue : Type
Queue = List Coord

Graph : Type
Graph = SortedMap (Coord, Coord) Nat

valid : Queue -> List Coord -> List Coord
valid q = filter (`elem` q)

nbrsStep : Graph -> Coord -> Coord -> (Preds, Distances) -> (Preds, Distances)
nbrsStep g u v pd@(p,d) = fromMaybe pd $ do
  alt <- [dis+len | dis <- lookup u d, len <- lookup (u, v) g]
  cur <- lookup v d
  pure $ if alt < cur
     then (insert v u p, insert v alt d)
     else (p, d)

swap : (a, b) -> (b, a)
swap (x,y) = (y,x)

least : Queue -> Distances -> Maybe (Coord, Queue)
least q d = let
    ds = map snd $ sort $ map swap $ toList d
  in do
    x <- head' $ filter (`elem` q) ds
    pure (x, delete x q)

step : Graph -> Queue -> (Preds,Distances) -> (Preds, Distances)
step g q pd = case least q $ snd pd of
               Nothing => pd
               Just (u,q') => let
                   ns = valid q $ neighbors u
                in step g q' $ foldr ($) pd $ map (nbrsStep g u) ns

conv : (Nat, Nat) -> (Int, Int)
conv (x,y) = (fromInteger $ natToInteger x, fromInteger $ natToInteger y)

getGraph : List (List Nat) -> Graph
getGraph = fromList . concat . mapi (\x => concat . mapi (\y, v =>
               [((n,conv (x,y)), v) | n <- neighbors $ conv (x,y)]))

getQueue : List (List Nat) -> Queue
getQueue = concat . mapi (\x, xs => mapi (\y,_ => conv (x,y)) xs)

run : Graph -> Queue -> (Preds, Distances)
run g q = let
    d = insert (0,0) 0 $ fromList $ map (\v => (v,99999999)) q
  in step g q (empty, d)

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl15/testinput2"
  let res' = forget $ map forget res
  printLn $ run (getGraph res') (getQueue res')


