module Fensterl19.Main

import Data.List
import Data.List1
import Data.Maybe
-- Debugging
import Data.String
import Debug.Trace

import Common.Input
import Fensterl19.Parser

Coords : Type
Coords = (Int, Int, Int)

withAll : (a -> List a -> b) -> List a -> List b
withAll f = go f []
  where
    go : (a -> List a -> b) -> List a -> List a -> List b
    go _ _  [] = []
    go f ys (x::xs) = f x (ys ++ xs) :: go f (x::ys) xs

diff : Coords -> Coords -> Coords
diff (ax, ay, az) (bx, by, bz) = (bx-ax, by-ay, bz-az)

sum : Coords -> Coords -> Coords
sum (ax, ay, az) (bx, by, bz) = (bx+ax, by+ay, bz+az)

mult : Coords -> Coords -> Coords
mult (ax, ay, az) (bx, by, bz) = (bx*ax, by*ay, bz*az)

data Swap = SAB | SBC | SAC | None | SBCA | SCAB
swap : Swap -> Coords -> Coords
swap SAB (a, b, c) = (b, a, c)
swap SBC (a, b, c) = (a, c, b)
swap SAC (a, b, c) = (c, b, a)
swap SBCA (a, b, c) = (b, c, a)
swap SCAB (a, b, c) = (c, a, b)
swap _ c = c

rotations : List Coords
rotations = zip3 [1,  1,  1,  1, -1, -1, -1, -1]
                 [1,  1, -1, -1,  1,  1, -1, -1]
                 [1, -1,  1, -1,  1, -1,  1, -1]

match : Coords -> List Coords -> List Coords -> Swap -> Coords -> Coords -> Maybe (List Coords)
match c orig other sw rot sample = let
  other' = map (\x => sum c (swap sw (mult rot (diff x sample)))) other
  news   = length $ filter (`elem` orig) other' -- trace (unlines $ map show $ zip (sort orig) $ sort other') $ 
  in if news >= 12 then Just other' else Nothing

tryRot : List Coords -> List Coords -> Coords -> Swap -> Coords -> Maybe (List Coords)
tryRot orig sample c sw rot = let
  match = head' $ mapMaybe (match c orig sample sw rot) sample
  in case match of
          Nothing => Nothing
          Just cs => Just $ nub $ orig ++ cs

trySwap : List Coords -> List Coords -> Coords -> Swap -> Maybe (List Coords)
trySwap orig sample c sw = head' $ mapMaybe (tryRot orig sample c sw) rotations

try : List Coords -> List Coords -> Coords -> Maybe (List Coords)
try orig sample c = head' $ mapMaybe (trySwap orig sample c) [None, SAB, SBC, SAC, SBCA, SCAB]

merge : List1 (List Coords) -> List (List Coords) -> List Coords
merge (x:::[]) [] = x
merge (x:::[]) xs = merge (x:::xs) []
merge (x:::y::xs) ys = case trace (show $ length xs) $ mapMaybe (try x y) x of
                            [] => merge (x:::xs) (y::ys)
                            x'::_ => merge (x':::xs) ys

manhattan : Coords -> Coords -> Int
manhattan (ax, ay, az) (bx, by, bz) = abs (bx-ax) + abs (by-ay) + abs (bz-az) 

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl19/input"
  printLn . length . forget $ res

  let res' = map snd res
  printLn $ List.length $ Main.merge res' []
