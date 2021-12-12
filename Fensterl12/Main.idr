module Fensterl12.Main

import Data.List
import Data.List1

import Common.Input
import Fensterl12.Parser
import Fensterl12.Support

-- Ex 1
convertImpl : List1 (String, String) -> Graph String
convertImpl = filterIllegal "start" "end" . convert

allowed : DepPred String
allowed p n = not $ isLower n && elem n p

continue : List1 String -> DepPred String -> Graph String
        -> List (List1 String)
continue p@("end":::_) _ _ = [p]
continue p@(n:::_) pr g = let
    p' = forget p
  in foldMap (\h => continue (h:::p') pr g) . filter (pr p') $ find n g

findPaths : DepPred String -> Graph String -> List (List1 String)
findPaths p = map reverse . continue ("start":::[]) p

-- Ex 2
noTwice : List String -> Bool
noTwice l = not $ any (\n => isLower n && count (==n) l > 1) l

twiceAllowed : DepPred String
twiceAllowed p n = noTwice p || allowed p n

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl12/input"
  --printLn res

  printLn $ length $ findPaths allowed $ convertImpl res

  printLn $ length $ findPaths twiceAllowed $ convertImpl res
