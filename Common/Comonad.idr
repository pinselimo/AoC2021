module Common.Comonad

public export
interface Functor w => Comonad w where
  extract : w a -> a
  duplicate : w a -> w (w a)
  extend : (w a -> b) -> w a -> w b

  duplicate g = extend id g
  extend f g = map f (duplicate g)

