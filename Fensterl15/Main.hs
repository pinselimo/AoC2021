{-# LANGUAGE TupleSections #-}
module Fensterl15.Main where
{- Same algorithm in Haskell
   intended to reflect the code in Idris as closely as possible
   Perfomance is significantly faster (without using any optimization flags)

   Used Integer to reflect any inefficiencies resulting from Idris' Nat
-}
import Data.List
import Data.List.Extra
import Data.Map
import qualified Data.Map as M
import Data.Set
import Data.Maybe

neg :: (Int, Int) -> (Int, Int)
neg (x,y) = (-x, -y)

type Coord = (Int, Int)

neighbors :: Coord -> [Coord]
neighbors (x, y) = Prelude.filter (\(x,y) -> x >= 0 && y >= 0) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

type Distances = Map Coord Integer
type Preds = Map Coord Coord
type Queue = Map (Integer, Coord) ()
type Graph = Map (Coord, Coord) Integer

nbrsStep :: Graph -> Coord -> Coord -> (Queue, Preds, Distances) -> (Queue, Preds, Distances)
nbrsStep g u v qpd@(q,p,d) = fromMaybe qpd $ do
  dis <- M.lookup u d
  len <- M.lookup (u, v) g
  cur <- M.lookup v d
  let alt = dis + len
  pure $ if alt < cur
     then (M.insert (alt,neg v) () q, M.insert v u p, M.insert v alt d)
     else qpd

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f xs = uncurry f <$> zip [0..length xs] xs

least :: Queue -> Maybe (Coord, Queue)
least q = (\k -> (neg $ snd k, M.delete k q)) . fst <$> safeHead (sort $ M.toList q)

step :: Graph -> (Queue,Preds,Distances) -> (Queue, Preds, Distances)
step g (q, p, d) = case least q of
               Nothing -> (q, p, d)
               Just (u,q') -> if u == (499, 499)
                      then (q, p, d)
                      else step g $ Prelude.foldr ($) (q', p, d)
                                  $ nbrsStep g u <$> neighbors u

conv :: (Integer, Integer) -> (Int, Int)
conv (x,y) = (fromIntegral x, fromIntegral y)

getGraph :: [[Integer]] -> Graph
getGraph = M.fromList . concat . mapi (\x -> concat . mapi (\y v ->
               [((n,conv (fromIntegral x, fromIntegral y)), v) | n <- neighbors $ conv (fromIntegral x,fromIntegral y)]))

getDists :: [[Integer]] -> Distances
getDists = M.insert (0,0) 0 . M.fromList . fmap (,99999999999) -- inf
         . concat . mapi (\x xs -> mapi (\y _ -> conv (fromIntegral x, fromIntegral y)) xs)

run :: Graph -> Distances -> (Preds, Distances)
run g d = case step g (M.fromList [((0, (0,0)), ())], M.empty, d) of
               (_, p, d) -> (p, d)

-- Ex 2
add' :: Integer -> Integer -> Integer
add' a b = let
  c = mod (a + b) 9
  in if c == 0 then 9 else c

enlarge1 :: Integer -> [Integer] -> [Integer]
enlarge1 n = concat . mapi (\i xs -> fmap (add' $ fromIntegral i) xs) . replicate (fromIntegral n)

enlarge :: Integer -> [[Integer]] -> [[Integer]]
enlarge n = transpose . fmap (enlarge1 n) . transpose . fmap (enlarge1 n)

main :: IO ()
main = do
  res <- readFile "Fensterl15/input"
  -- who's to say I can't parse in a single line? ;)
  let ls = (Prelude.map (read . (:[])) <$> lines res) :: [[Integer]]
  let big = enlarge 5 ls
  print $ last $ M.toList $ snd $ run (getGraph big) (getDists big)

