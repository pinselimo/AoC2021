module Fensterl11.Main

import Data.List1
import Data.List
import Data.Maybe
import Data.Vect

-- Animation
import System

import Common.Input
import Fensterl11.Parser
import Fensterl11.Types

Coord : Type
Coord = (Nat, Nat)

dimNeighbors : Nat -> List Nat
dimNeighbors (S a) = [a..a+2]
dimNeighbors _ = [0, 1]

neighborIdcs : Coord -> List Coord
neighborIdcs (x, y) = [(x',y')
  | x' <- dimNeighbors x
  , y' <- dimNeighbors y
  , x/=x' || y/=y'
  ]

neighbors : {n,m:Nat} -> Grid n m a -> Coord -> List a
neighbors g = mapMaybe (g!!) . neighborIdcs

nbrs : {n, m: Nat} -> Grid n m a -> (Fin n, Fin m) -> List a
nbrs g (x,y) = neighbors g (finToNat x, finToNat y)

nbrsFlashed : {n: Nat} -> Ocean n (Octo a) -> (Fin n, Fin n) -> Nat
nbrsFlashed g c = count recentFlash $ nbrs g c

init : Ocean n a -> Ocean n (Octo a)
init = map (MkOcto False False)

step1 : (Num a, Ord a) => Octo a -> Octo a
step1 (MkOcto _ _ n) = let n' = 1 + n in if n' > 9
                               then MkOcto True True 0
                               else MkOcto False False n'

step2 : {n:Nat} -> Ocean n (Octo Nat) -> (Fin n, Fin n) -> Octo Nat -> Octo Nat
step2 oc cs (MkOcto _ True n) = MkOcto False True 0
step2 oc cs (MkOcto _ False ps) = let
  ps' = ps + nbrsFlashed oc cs
  in if ps' > 9 then MkOcto True True 0 else MkOcto False False ps'

noFlashes : Ocean n (Octo a) -> Bool
noFlashes (MkGrid gs) = not . any id . map (any recentFlash) $ gs

tick' : {n:Nat} -> Ocean n (Octo Nat) -> Ocean n (Octo Nat)
tick' o = if noFlashes o then o else tick' $ mapWithCoords (step2 o) o

totalFired : Ocean n (Octo a) -> Nat
totalFired (MkGrid g) = sum . map (count hasFlashed) $ g

tick : {n:Nat} -> Ocean n (Octo Nat) -> (Nat, Ocean n (Octo Nat))
tick g = let
  g'= tick' (map step1 g)
  c = totalFired g'
  in (c, g')

-- Ex 1
simulation : {n:Nat} -> Nat -> Ocean n (Octo Nat) -> (Nat, Ocean n (Octo Nat))
simulation 0 g = (0, g)
simulation (S n) g = let
    (k, g') = tick g
  in mapFst (k+) $ simulation n g'

-- Ex 2
allFlashed : Ocean n (Octo a) -> Bool
allFlashed (MkGrid gs) = all id . map (all hasFlashed) $ gs

synchro : {n:Nat} -> Nat -> Ocean n (Octo Nat) -> (Nat, Ocean n (Octo Nat))
synchro n g = if allFlashed g then (n, g) else synchro (S n) . snd . tick $ g

-- Animation
animate : HasIO io => {n:Nat} -> Nat -> Ocean n (Octo Nat) -> io ()
animate n g = let
  (k, g') = tick g
  in putStr "\x1B[12A"
  >> printLn g'
  >> putStrLn ("\x1B[31mTick " ++ show n
              ++ "\x1B[32m  Flashed " ++ show k
              ++ "\x1B[37m")
  >> usleep 200000
  >> animate (S n) g'



main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl11/input"
  let mGrid = map init . gridFromList 10 $ map forget . forget $ res
  case mGrid of
       Nothing => printLn "Error"
       Just grid => do
         -- Ex 1
         printLn . fst $ simulation 100 grid
         -- Ex 2
         printLn . fst $ synchro 0 grid

         -- Animation
         putStrLn ""
         printLn grid
         putStrLn "Let's go"
         animate 0 grid
