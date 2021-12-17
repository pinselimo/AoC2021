module Fensterl17.Main

import Data.List
import Data.List1
import Debug.Trace

import Common.Input
import Fensterl17.DataParser

Coord : Type
Coord = (Int, Int)

Vel : Type
Vel = (Int, Int)

step : (Vel, Coord) -> (Vel, Coord)
step ((vx, vy), (x, y)) = case (vx > 0, vx < 0) of
  (False, False) => ((vx, vy-1), (x, y+vy))
  (True , False) => ((vx-1, vy-1), (x+vx, y+vy))
  (False,  True) => ((vx+1, vy-1), (x+vx, y+vy))
  _ => ((vx, vy-1), (x, y+vy)) -- can never happen

maxVelocityY : Int -> Int -> Int
maxVelocityY upper lower = go 0 upper lower
  where
    go : Int -> Int -> Int -> Int
    go n u l = if sum [l..n] > u then n-1 else go (n+1) u l

allVelY : Int -> Int -> List Int
allVelY upper lower = nub $ go lower (maxVelocityY upper lower)
  where
    go : Int -> Int -> List Int
    go s e = let
          f : Int -> Int
          f = if e>=upper then (1+) else (\x => x-1)
        in if s < lower
          then []
          else let
        sm = sum [s..e]
        in if sm < lower
          then go (f s) e
          else if sm > upper
          then go s (e-1)
          else e :: (go s (e-1))

minVelocityX : Int -> Int
minVelocityX xmin = let
  vmin = go 0 (abs xmin)
  in if xmin < 0 then (-1) * vmin else vmin
  where
    go : Int -> Int -> Int
    go n l = if sum [0..n] >= l then n else go (n+1) l

allVelX : Int -> Int -> List Int
allVelX lower upper = go 1 (minVelocityX lower)
  where
    go : Int -> Int -> List Int
    go s e = let sm = sum [s..e]
        in   if s > upper 
        then [] 
        else if sm > upper
        then go (s+1) e
        else let es = go s (e+1) 
        in   if sm < lower
        then es
        else e :: es

sim : (Int, Int) -> (Int, Int) -> Vel -> Bool
sim (xmin, xmax) (ymax, ymin) v = go (v, (0,0))
  where
    go : (Vel, Coord) -> Bool
    go p@(_, (x,y)) = if xmin <= x && x <= xmax
                    && ymin <= y && y <= ymax
                    then True
                    else if x > xmax || y < ymin then False
                    else go $ step p

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl17/testdata"
  let theirs = map (\(a,b) => (fromInteger a, fromInteger b)) $ sort $ forget res
  -- Ex 1
  printLn $ sum [0..maxVelocityY (-63) (-115)]

  printLn $ sort $ allVelX 20 30
  printLn $ sort $ allVelY (-5) (-10)

  let mine = filter (sim (20, 30) (-5, -10))
           $ [ (x,y)
             | x <- allVelX 20 30
             , y <- allVelY (-5) (-10)
             ]
  printLn $ length theirs
  printLn $ length mine
  printLn $ [m | m <- theirs, not (elem m mine)]
