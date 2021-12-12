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

filterImpossible : List String -> List String -> List String
filterImpossible p = filter (\n => not $ toLower n == n && elem n p)

continue : List1 String -> Graph String -> List (List1 String)
continue p@("end":::_) _ = [p]
continue (n:::p) g = concat . map ((`continue` g) . (:::n::p))
                   . filterImpossible p $ find n g

findPaths : Graph String -> List (List1 String)
findPaths = map reverse . continue ("start":::[])

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl12/input"
  --printLn res

  printLn $ length $ findPaths $ convertImpl res

