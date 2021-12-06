module Fensterl6.Main

import Data.List1
import Data.Vect

import AoC.Input
import Fensterl6.Parser


-- Simple, awfully slow solution
simpleStep : List Int -> List Int
simpleStep [] = [] 
simpleStep (x::xs) = if x == 0
                  then 6 :: simpleStep xs ++ [8]
                  else (x-1) :: simpleStep xs

simpleSim : Nat -> List Int -> List Int
simpleSim 0 = id
simpleSim (S n) = simpleSim n . simpleStep

data St : Type where
  MkSt : Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> St

collect : List Nat -> St
collect [] = MkSt 0 0 0 0 0 0 0 0 0
collect (x::xs) = let
    xs' = collect xs
  in case xs' of
    MkSt a b c d e f g h i =>
      case x of
        0 => MkSt (S a) b c d e f g h i
        1 => MkSt a (S b) c d e f g h i
        2 => MkSt a b (S c) d e f g h i
        3 => MkSt a b c (S d) e f g h i
        4 => MkSt a b c d (S e) f g h i
        5 => MkSt a b c d e (S f) g h i
        6 => MkSt a b c d e f (S g) h i
        _ => xs'

stStep : St -> St
stStep (MkSt a b c d e f g h i) = MkSt b c d e f g (h+a) i a

sumSt : St -> Nat
sumSt (MkSt a b c d e f g h i) = a+b+c+d+e+f+g+h+i

stSim : Nat -> List Nat -> St
stSim 0 = collect
stSim (S n) = stStep . stSim n

-- Using vectors
vColl : List Nat -> Maybe (Vect 9 Nat)
vColl [] = pure $ replicate 9 0
vColl (x::xs) = do
  v <- vColl xs
  x' <- natToFin x 9
  pure $ (updateAt x' S) v

vStep : Vect 9 Nat -> Vect 9 Nat
vStep v = let
    reprods = head v
  in updateAt 6 (reprods+) $ tail v ++ pure reprods

vSim : Nat -> Vect 9 Nat -> Vect 9 Nat
vSim 0 = id
vSim (S k) = vStep . vSim k



main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl6/input"
  printLn res

  -- Ex1
  --printLn . length . simpleSim 80 . forget . map cast $ res

  printLn . sumSt . stSim 80 . forget $ res

  -- Ex2
  printLn . sumSt . stSim 256 . forget $ res


  -- Using vector
  case vColl $ forget res of
       Nothing => printLn res
       Just v  => do
         printLn . sum . vSim 80 $ v -- Ex1
         printLn . sum . vSim 256 $ v -- Ex2
        
