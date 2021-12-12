module Fensterl12.Main

import Data.List
import Data.List1
import Data.Maybe
import Data.SortedMap
import Data.String

import Common.Input
import Fensterl12.Parser

Graph : Type -> Type
Graph ty = SortedMap ty (List ty)

find : k -> SortedMap k (List v) -> List v
find k = lowerMaybe . lookup k

swap : (a, b) -> (b, a)
swap (x, y) = (y, x)

put : (a, a) -> Graph a -> Graph a
put p = put' (swap p) . put' p
  where
    put' : (a, a) -> Graph a -> Graph a
    put' (src, dst) m = insert src (dst::find src m) m

-- Remove any edges from "end" and to "start"
filterIllegal : Eq a => a -> a -> Graph a -> Graph a
filterIllegal start end = delete end . map (filter (/=start))

convert : Ord a => List1 (a, a) -> Graph a
convert = foldr put empty

-- Ex 1
convertImpl : List1 (String, String) -> Graph String
convertImpl = filterIllegal "start" "end" . convert

DepPred : Type -> Type
DepPred a = List a -> a -> Bool

forbidden : DepPred String
forbidden p n = not $ toLower n == n && elem n p

continue : List1 String -> DepPred String -> Graph String
        -> List (List1 String)
continue p@("end":::_) _ _ = [p]
continue p@(n:::_) pr g = let
    p' = forget p
  in concat . map ((\x => continue x pr g) . (:::p')) . filter (pr p') $ find n g

findPaths : DepPred String -> Graph String -> List (List1 String)
findPaths p = map reverse . continue ("start":::[]) p

-- Ex 2
hasTwice : List String -> Bool
hasTwice l = let
    l' = filter (\n => toLower n == n) l
  in any (\n => count (==n) l' > 1) l'

twiceAllowed : DepPred String 
twiceAllowed p n = (not $ toLower n == n && hasTwice p) || forbidden p n 

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl12/input"
  --printLn res

  printLn $ length $ findPaths forbidden $ convertImpl res

  printLn $ length $ findPaths twiceAllowed $ convertImpl res
