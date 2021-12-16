module Fensterl15.Main
-- Let's just try Dijkstra
import Data.List1
import Data.List
import Data.List.Extra
import Data.SortedMap
import Data.SortedSet
import Data.Maybe
import Data.Nat
import Control.Monad.State

import Common.Input
import Fensterl11.Parser

neg : (Int, Int) -> (Int, Int)
neg (x,y) = (-x, -y)

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
  dis <- lookup u d
  len <- lookup (u, v) g
  cur <- lookup v d
  let alt = dis + len
  pure $ if alt < cur
     then (insert (alt,neg v) () q, insert v u p, insert v alt d)
     else qpd

least : Queue -> Maybe (Coord, Queue)
least q = (\k => (neg $ snd k, delete k q)) . fst <$> leftMost q

step : Graph -> (Queue,Preds,Distances) -> (Queue, Preds, Distances)
step g (q, p, d) = case least q of
               Nothing => (q, p, d)
               Just (u,q') => if u == (499, 499)
                      then (q, p, d)
                      else step g $ foldr ($) (q', p, d) $ map (nbrsStep g u) $ neighbors u

conv : (Nat, Nat) -> (Int, Int)
conv (x,y) = (fromInteger $ natToInteger x, fromInteger $ natToInteger y)

getGraph : List (List Nat) -> Graph
getGraph = fromList . concat . mapi (\x => concat . mapi (\y, v =>
               [((n,conv (x,y)), v) | n <- neighbors $ conv (x,y)]))

getDists : List (List Nat) -> Distances
getDists = insert (0,0) 0 . fromList . map (,99999999999) -- inf
         . concat . mapi (\x, xs => mapi (\y,_ => conv (x,y)) xs)

run : Graph -> Distances -> (Preds, Distances)
run g d = snd $ step g (fromList [((0, (0,0)), ())], empty, d)

-- Ex 2
add' : Nat -> Nat -> Nat
add' a b = let
  c = mod (a + b) 9
  in if c == 0 then 9 else c

enlarge1 : Nat -> List Nat -> List Nat
enlarge1 n = concat . mapi (\i, xs => map (add' i) xs) . (replicate n)

enlarge : Nat -> List (List Nat) -> List (List Nat)
enlarge n = transpose . map (enlarge1 n) . transpose . map (enlarge1 n)

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl15/input"
  let res' = forget $ map forget res
  --printLn $ run (getGraph res') (getDists res')

  let big = enlarge 5 res'
  printLn $ run (getGraph big) (getDists big)

