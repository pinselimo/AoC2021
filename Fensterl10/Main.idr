module Fensterl10.Main

import Control.Monad.State
import Control.Monad.State.State
import Control.Monad.State.Interface
import Data.List
import Data.List1

import Common.Input
import Fensterl10.Parser

data Parens : Type where
  Mismatched : String -> String -> Parens
  Missing : String -> Parens

Show Parens where
  show (Mismatched got exp) = got ++ " /= " ++ exp
  show (Missing exp) = "no " ++ exp

op : String -> Maybe String
op = (`lookup` [("<", ">"), ("{", "}"), ("[", "]"), ("(", ")")])

score : String -> Nat
score s = let
  ps = lookup s [(">", 25137), ("}", 1197), ("]", 57), (")", 3)]
  in case ps of
          Nothing => 0
          Just n  => n

scoreP : Parens -> Nat
scoreP (Mismatched m _) = score m
scoreP _ = 0

tidy : List String -> State (List String) (List Parens)
tidy [] = get >>= pure . map Missing
tidy (next::todo) = let
  shouldMatch : String -> State (List String) (List Parens -> List Parens)
  shouldMatch = pure . (::) . Mismatched next
  in case op next of
    Just closing => get >>= put . (closing ::) >> tidy todo
    Nothing => do
      state <- get
      case state of
           [] => shouldMatch "" <*> tidy todo
           (exp::r) => put r >>
                    if next == exp
                      then tidy todo
                      else shouldMatch exp <*> tidy todo

tidyup : List String -> List Parens
tidyup s = evalState [] (tidy s)

-- Ex1
getMism : List Parens -> Maybe Parens
getMism (m@(Mismatched _ _)::_) = Just m
getMism _ = Nothing


main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl10/input"
  --printLn res

  printLn . sum . map scoreP . mapMaybe getMism . map tidyup . forget $ res

