module Fensterl10.Main

import Control.Monad.State
import Data.List
import Data.List1
import Data.Nat
import Data.Maybe

import Common.Input
import Fensterl10.Parser

data Parens : Type where
  Mismatched : String -> Parens
  Missing : String -> Parens

op : String -> Maybe String
op = (`lookup` [("<", ">"), ("{", "}"), ("[", "]"), ("(", ")")])

mismatchT : String -> Maybe Nat
mismatchT = (`lookup` [(">", 25137), ("}", 1197), ("]", 57), (")", 3)])

missingT : String -> Maybe Nat
missingT = (`lookup` [(">", 4), ("}", 3), ("]", 2), (")", 1)])

score : (String -> Maybe Nat) -> String -> Nat
score f s = case f s of
          Nothing => 0
          Just n  => n

scoreP : Parens -> Nat
scoreP (Mismatched m) = score mismatchT m
scoreP (Missing m) = score missingT m

mismatch : String -> State (List String) (List Parens -> List Parens)
mismatch = pure . (::) . Mismatched

tidy : List String -> State (List String) (List Parens)
tidy [] = get >>= pure . map Missing
tidy (next::todo) = case op next of
    Just closing => get >>= put . (closing ::) >> tidy todo
    Nothing => do
      state <- get
      put (fromMaybe [] $ tail' state)
      if fromMaybe False ((==next) <$> head' state)
         then tidy todo
         else mismatch next <*> tidy todo

tidyup : List String -> List Parens
tidyup s = evalState [] (tidy s)

-- Ex1
getMism : List Parens -> Maybe Parens
getMism (m@(Mismatched _)::_) = Just m
getMism _ = Nothing

-- Ex2
getMiss : List Parens -> Maybe (List Parens)
getMiss (m@(Mismatched _)::_) = Nothing
getMiss xs = Just xs

scoreMissing : Nat -> Parens -> Nat
scoreMissing n p = scoreP p + 5 * n

scoreMLine : List Parens -> Nat
scoreMLine = foldl scoreMissing 0

median : Ord a => List a -> Maybe a
median xs = let
  half = div (length xs) 2
  in case drop half $ sort xs of
          (x::_) => Just x
          _ => Nothing

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl10/input"
  --printLn res

  -- Ex 1
  printLn . sum . map scoreP . mapMaybe getMism . map tidyup . forget $ res

  -- Ex 2
  printLn . median
          . map scoreMLine . mapMaybe getMiss . map tidyup . forget $ res

