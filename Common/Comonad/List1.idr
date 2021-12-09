module Common.Comonad.List1

import Data.List1

import Common.Comonad

export
Comonad List1 where
  extract (a ::: _) = a
  duplicate w@(a ::: aas) = w ::: (case aas of
                               [] => []
                               (a'::as) => forget $ duplicate (a':::as))

