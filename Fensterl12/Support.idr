module Fensterl12.Support

import Data.List
import Data.List1
import Data.SortedMap
import Data.String
import Data.Maybe

public export
DepPred : Type -> Type
DepPred a = List a -> a -> Bool

public export
Graph : Type -> Type
Graph ty = SortedMap ty (List ty)

export
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
export
filterIllegal : Eq a => a -> a -> Graph a -> Graph a
filterIllegal start end = delete end . map (filter (/=start))

export
convert : Ord a => List1 (a, a) -> Graph a
convert = foldr put empty

export
isLower : String -> Bool
isLower s = toLower s == s

