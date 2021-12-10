module Fensterl10.Fold

import Data.List
import Data.List1
import Data.Nat
import Data.Maybe

import Common.Input
import Fensterl10.Parser

tidyup : List String -> List String
tidyup s = foldl f [] s
  where
    f : List String -> String -> List String
    f xs "(" = "("::xs
    f xs "[" = "["::xs
    f xs "{" = "{"::xs
    f xs "<" = "<"::xs

    f ("("::xs) ")" = xs
    f ("["::xs) "]" = xs
    f ("{"::xs) "}" = xs
    f ("<"::xs) ">" = xs
                       
    f (x::xs) c = xs ++ [c]
    f [] c = []

mismatch : List String -> Maybe String
mismatch = head' . filter (`elem` [">", "}", "]", ")"])

hasMismatch : List String -> Bool
hasMismatch = (==1) . length . filter (`elem` [">", "}", "]", ")"])

mismatchT : String -> Maybe Nat
mismatchT = (`lookup` [(">", 25137), ("}", 1197), ("]", 57), (")", 3)])

missingT : String -> Maybe Nat
missingT = (`lookup` [("<", 4), ("{", 3), ("[", 2), ("(", 1)])

score : (String -> Maybe Nat) -> String -> Nat
score f s = case f s of
          Nothing => 0
          Just n  => n

scoreMissing : Nat -> String -> Nat
scoreMissing n s = score missingT s + 5 * n

scoreMLine : List String -> Nat
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
  let tidyList = map tidyup . forget $ res
  printLn . sum . map (score mismatchT) . mapMaybe mismatch $ tidyList

  printLn . median . map scoreMLine . filter (not . hasMismatch) $ tidyList

