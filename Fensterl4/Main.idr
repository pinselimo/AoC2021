module Fensterl4.Main

import Control.Monad.Writer
import Control.Monad.Writer.Interface
import Control.Monad.Identity
import Data.List
import Data.List1
import Data.Vect

import Common.Input
import Fensterl4.Parser
import Fensterl4.Types

Pred : Type -> Type
Pred a = a -> Bool

checkDir : Pred a -> Vect n (Vect m a) -> Bool
checkDir p = any (all p)

checkWin : {m:Nat} -> Pred a -> Grid n m a -> Bool
checkWin p (MkGrid g) = checkDir p g || checkDir p (transpose g)

countScore : Num b => (a -> b) -> Grid n m a -> b
countScore f (MkGrid g) = sum . map (sum . map f) $ g

score : Num b => Pred a -> (a -> b) -> Grid n m a -> b
score p f g = let
    h : Pred a -> a -> b
    h p x = if p x then f x else 0
  in countScore (h (not . p)) g

markAll : Eq a => a -> Bingo n a -> Bingo n a
markAll x (MkGrid g) = MkGrid $ map (map (mark x)) g

game : Eq a => {n : Nat} -> List a -> List (Bingo n a) ->
       Writer (List (a, Bingo n a)) ()
game [] _ = pure ()
game (x::xs) grids = let
    grids' = map (markAll x) grids
    winners = filter (checkWin chk) grids'
    losers  = filter (not . checkWin chk) grids'
  in case winners of
          (w::_) => tell [(x, w)]
                 >> game xs losers
          _ => game xs grids'

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl4/input"
  --printLn res
  let result : List (Nat, Bingo 5 Nat)
      result = case res of
            (_, Nothing) => []
            (rands, Just gs) => execWriter $ game rands (forget gs)

  case result of
       [] => putStrLn "No winner could be determined."
       (l::ls) => let
              lst = last (l:::ls)
            in do
              printLn $ calc l
              printLn $ calc lst -- Ex 2
            where
              calc : (Nat, Bingo 5 Nat) -> Nat
              calc (n,g) = n * score chk get g

