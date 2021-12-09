module Fensterl05.Main

import Data.List1
import Data.List

-- For second variant
import Control.Monad.State
import Data.Maybe
import Data.SortedMap

import Common.Input
import Fensterl05.Parser

data Dir : Type where
  Horizontal : Dir
  Vertical : Dir
  Diagonal : Dir

isHVD : Eq n => Vent n -> Dir
isHVD (MkVent (x1, y1) (x2, y2)) = case (x1 == x2, y1 == y2) of
                                        (True, _) => Horizontal
                                        (_, True) => Vertical
                                        _         => Diagonal

ventCoverage : (Range n, Eq n) => Bool -> Vent n -> List (n, n)
ventCoverage diagonal v@(MkVent (x1, y1) (x2, y2)) = let
        xs = [x1..x2]
        ys = [y1..y2]
        in case isHVD v of
             Horizontal => (x1,) <$> ys
             Vertical   => (,y1) <$> xs
             Diagonal   => ifThenElse diagonal
                             (zipWith (,) xs ys)
                             []

fullCoordinates : (Range n, Eq n) => Bool -> List1 (Vent n) -> List (n, n)
fullCoordinates d = concat . map (ventCoverage d)

nDangerous : (Range n, Eq n, Ord n) => List (n, n) -> Nat
nDangerous = length . filter (>=2) . map (length . forget) . group . sort

-- Using State and SortedMap
recordField : Eq n => (n, n) -> SortedMap (n, n) Nat -> SortedMap (n, n) Nat
recordField k m = insert k (S $ fromMaybe 0 $ lookup k m) m

recordVent : (Range n, Eq n) =>
             Bool -> Vent n -> State (SortedMap (n, n) Nat) ()
recordVent d v = for_ (ventCoverage d v) (modify . recordField)

fieldPops : (Range n, Eq n, Traversable t) =>
            Bool -> t (Vent n) -> State (SortedMap (n, n) Nat) ()
fieldPops d = traverse_ $ traverse_ (modify . recordField) . ventCoverage d

countDangerous : (Ord a, Num a) => SortedMap (Nat, Nat) a -> Nat
countDangerous = length . filter (>=2) . values

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl05/input"
  -- Ex 5.1
  printLn . nDangerous . fullCoordinates False $ res
  -- Ex 5.2
  printLn . nDangerous . fullCoordinates True  $ res

  -- State and SortedMap
  printLn $ countDangerous $ execState empty (fieldPops False res)

  printLn $ countDangerous $ execState empty (fieldPops True res)

