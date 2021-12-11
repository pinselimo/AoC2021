module Fensterl11.Types

import Data.String
import Data.Vect
import Data.Vect.Extra

public export
data Grid : Nat -> Nat -> Type -> Type where
  MkGrid : Vect n (Vect m a) -> Grid n m a

export
Functor (Grid n m) where
  map f (MkGrid g) = MkGrid $ map (map f) g

public export
data Octo : Type -> Type where
  MkOcto : Bool -> Bool -> a -> Octo a

export
Functor Octo where
  map f (MkOcto r b a) = MkOcto r b $ f a

-- Deprecated
export
defaultOcto : a -> Octo a -> a
defaultOcto x (MkOcto _ b y) = if b then x else y

export
hasFlashed : Octo a -> Bool
hasFlashed (MkOcto _ b _) = b

export
recentFlash : Octo a -> Bool
recentFlash (MkOcto r _ _) = r

export
Show a => Show (Octo a) where
  show (MkOcto _ True _) = "\x1B[36m+\x1B[37m"
  show (MkOcto _ _ n) = padLeft 1 ' ' $ show n

vectToList : Vect n a -> List a
vectToList = foldr (::) []

export
Show a => Show (Grid n m a) where
  show (MkGrid xs) = unlines . vectToList
                   $(unwords . vectToList . map show) <$> xs

public export
Ocean : Nat -> Type -> Type
Ocean n a = Grid n n a

export
gridFromList : (n:Nat) -> (xs : List (List a)) -> Maybe (Ocean n a)
gridFromList m xs = let
    xs' = traverse (toVect m) xs >>= toVect m
  in MkGrid <$> xs'

infix 6 !!
export
(!!) : {n, m:Nat} -> Grid n m a -> (Nat, Nat) -> Maybe a
(MkGrid g) !! (x,y) = do
    x' <- natToFin x n
    y' <- natToFin y m
    pure $ index y' (index x' g)

export
mapWithCoords : ((Fin n, Fin m) -> a -> b) -> Grid n m a -> Grid n m b
mapWithCoords f (MkGrid g) = let
  u : Fin n -> (Fin m, a) -> b
  u x (y, elem) = f (x,y) elem
  in MkGrid . mapWithPos (map . u) . map (mapWithPos (,)) $ g

