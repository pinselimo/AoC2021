module Common.Comonad.Store

import Common.Comonad

public export
data Store : s -> a -> Type where
  MkStore : (s -> a) -> s -> Store s a

export
Functor (Store s) where
  map f (MkStore g x) = MkStore (f . g) x

export
Comonad (Store s) where
  extract (MkStore f x) = f x
  duplicate (MkStore f x) = MkStore (\y => MkStore f y) x
  extend f s@(MkStore g x) = MkStore (f . MkStore g) x

export
pos : Store s a -> s
pos (MkStore _ idx) = idx

export
seek : s -> Store s a -> Store s a
seek idx (MkStore f _) = MkStore f idx

export
peek : s -> Store s a -> a
peek idx (MkStore f _) = f idx
