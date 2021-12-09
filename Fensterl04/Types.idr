module Fensterl4.Types

import Data.String
import Data.Vect

public export
data Grid : Nat -> Nat -> Type -> Type where
  MkGrid : Vect n (Vect m a) -> Grid n m a

export
data Field : Type -> Type where
  Unmarked : Num a => a -> Field a
  Marked : Num a => a -> Field a

MkField : Num a => a -> Field a
MkField = Unmarked

export
get : Field a -> a
get (Unmarked x) = x
get (Marked x) = x

export
Show a => Show (Field a) where
  show = show . get

export
Show a => Show (Grid n m a) where
  show (MkGrid xs) = foldr (\a, b => a ++ "\n" ++ b) "" $ map show xs

export
chk : Field a -> Bool
chk (Marked _) = True
chk _ = False

export
mark : Eq a => a -> Field a -> Field a
mark y ux@(Unmarked x) = if x == y then Marked x else ux
mark _ mx = mx

public export
Bingo : Nat -> Type -> Type
Bingo n a = Grid n n (Field a)

export
gridFromList : Num a => (n:Nat) -> (xs : List (List a)) -> Maybe (Bingo n a)
gridFromList m xs = let
    xs' = for (map (map MkField) xs) (toVect m) >>= toVect m
  in MkGrid <$> xs'

