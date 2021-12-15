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
Queue = SortedMap (Nat, Coord) ()

Graph : Type
Graph = SortedMap (Coord, Coord) Nat

nbrsStep : Graph -> Coord -> Coord -> (Queue, Preds, Distances) -> (Queue, Preds, Distances)
nbrsStep g u v qpd@(q,p,d) = fromMaybe qpd $ do
  alt <- [dis+len | dis <- lookup u d, len <- lookup (u, v) g]
  cur <- lookup v d
  pure $ if alt < cur
     then (insert (alt,v) () q, insert v u p, insert v alt d)
     else qpd

swap : (a, b) -> (b, a)
swap (x,y) = (y,x)

least : Queue -> Maybe (Coord, Queue)
least q = do
    (k@(_,h), _) <- leftMost q
    pure (h, delete k q)

step : Graph -> (Queue,Preds,Distances) -> (Queue, Preds, Distances)
step g (q, p, d) = case least q of
               Nothing => (q, p, d)
               Just (u,q') => let
                   ns = neighbors u
                in step g $ foldr ($) (q', p, d) $ map (nbrsStep g u) ns

conv : (Nat, Nat) -> (Int, Int)
conv (x,y) = (fromInteger $ natToInteger x, fromInteger $ natToInteger y)

getGraph : List (List Nat) -> Graph
getGraph = fromList . concat . mapi (\x => concat . mapi (\y, v =>
               [((n,conv (x,y)), v) | n <- neighbors $ conv (x,y)]))

getDists : List (List Nat) -> Distances
getDists = insert (0,0) 0 . fromList . map (,999999999)
         . concat . mapi (\x, xs => mapi (\y,_ => conv (x,y)) xs)

run : Graph -> Distances -> (Preds, Distances)
run g d = snd $ step g (fromList [((0, (0,0)), ())], empty, d)

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl15/testinput2"
  let res' = forget $ map forget res
  printLn $ run (getGraph res') (getDists res')


