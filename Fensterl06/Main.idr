module Fensterl06.Main

import Data.List1
import Data.Vect

import Common.Input
import Fensterl06.Parser


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
vColl : {n:Nat} -> Nat -> List Nat -> Maybe ((Vect (S n) Nat), Fin (S n))
vColl ix [] = natToFin ix (S n) >>= pure . (replicate (S n) 0,)
vColl ix (x::xs) = do
  (v, ix') <- vColl ix xs
  x' <- natToFin x (S n)
  pure ((updateAt x' S) v, ix')

plusOneSucc : (n : Nat) -> S n = n + 1
plusOneSucc 0 = Refl
plusOneSucc (S k) = cong S (plusOneSucc k)

vStep : {n:Nat} -> Fin (S n) -> Vect (S n) Nat -> Vect (S n) Nat
vStep omega v = let
    repr = head v
  in updateAt omega (repr+)
   $ rewrite plusOneSucc n in tail v ++ pure repr

vSim : {n:Nat} -> Nat -> Fin (S n) -> Vect (S n) Nat -> Vect (S n) Nat
vSim 0 _ = id
vSim (S k) omega = vStep omega . vSim k omega


main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl06/input"

  -- Ex1
  --printLn . length . simpleSim 80 . forget . map cast $ res

  printLn . sumSt . stSim 80 . forget $ res

  -- Ex2
  printLn . sumSt . stSim 256 . forget $ res


  -- Using vector
  case vColl 6 $ forget res of
       Nothing => printLn res
       Just (v, ix) => do
         printLn . sum . vSim {n=8} 80 ix $ v -- Ex1
         printLn . sum . vSim {n=8} 256 ix $ v -- Ex2

