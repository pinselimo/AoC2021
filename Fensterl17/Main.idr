module Fensterl17.Main

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

-- This is the longest we can fly to hit our target
startVelocityX : Int -> Int
startVelocityX xmax = let
  vmax = go 0 (abs xmax)
  in if xmax < 0 then (-1) * vmax else vmax
  where
    go : Int -> Int -> Int
    go n u = if sum [0..n] >= u then n-1 else go (n+1) u

minVelocityX : Int -> Int
minVelocityX xmin = let
  vmin = go 0 (abs xmin)
  in if xmin < 0 then (-1) * vmin else vmin
  where
    go : Int -> Int -> Int
    go n l = if sum [0..n] >= l then n else go (n+1) l

maxVelocityY : Int -> Int -> Int
maxVelocityY upper lower = go 0 upper lower
  where
    go : Int -> Int -> Int -> Int
    go n u l = if sum [l..n] > u then n-1 else go (n+1) u l

maxHeight : Vel -> Int
maxHeight v = maxHeight' (v, (0,0))
  where
    maxHeight' : (Vel, Coord) -> Int
    maxHeight' d@(_, (_, y)) = let
      d'@(_, (_, y')) = step d
      in if y' < y then y else maxHeight' d'

main : HasIO io => io ()
main = do
  -- Ex 1
  printLn $ maxHeight (startVelocityX 263, maxVelocityY (-63) (-115))

