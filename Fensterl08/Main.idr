module Fensterl08.Main

import Data.List
import Data.List1
import Data.Maybe
import Data.SortedMap

import Common.Input
import Fensterl08.Parser

-- Ex 1
outValsLen : (List1 String, List1 String) -> List1 Nat
outValsLen = map length . snd

isSimple : Nat -> Bool
isSimple n = n `elem` [ 2 -- One
                      , 3 -- Seven
                      , 4 -- Four
                      , 7 -- Eight
                      ]

countSimple : List1 Nat -> Nat
countSimple = count isSimple

-- Ex 2
Code : Type
Code = List Char

code : String -> Code
code = unpack

infixr 6 ~~
(~~): Code -> Code -> Bool
(~~) a = foldr (\e, ac => (e `elem` a) && ac) True

equals : Code -> Code -> Bool
equals a b = a ~~ b && b ~~ a

diff : Code -> Code -> Code
diff u a = foldr (\e, ac => if e `elem` a then ac else e::ac) [] u

finished : SortedMap Nat Code -> Bool
finished m = all (`elem` keys m) [0..9]

fromFive : Code -> SortedMap Nat Code -> Maybe Nat
fromFive c m = let
  five  = (c~~) <$> (pure diff <*> lookup 4 m <*> lookup 1 m)
  three = (c~~) <$> lookup 7 m
  in case (three, five) of
          (Just True, _)   => Just 3
          (_, Just True)   => Just 5
          (Just _, Just _) => Just 2
          _ => Nothing

fromSix : Code -> SortedMap Nat Code -> Maybe Nat
fromSix c m = let
  nine = (c~~) <$> lookup 3 m
  six  = (c~~) <$> lookup 5 m
  in case (nine, six) of
          (Just True, _)      => Just 9
          (Just _, Just True) => Just 6
          (Just _, Just _)    => Just 0
          _ => Nothing

fromCode : Code -> SortedMap Nat Code -> SortedMap Nat Code
fromCode c m = let
    put : Nat -> SortedMap Nat Code
    put d = insert d c m
  in if finished m
     then m
     else case length c of
           2 => put 1
           3 => put 7
           4 => put 4
           5 => fromMaybe m $ put <$> fromFive c m
           6 => fromMaybe m $ put <$> fromSix c m
           7 => put 8
           _ => m

getDigitMap : List1 String -> List (Nat, Code)
getDigitMap l = let
    input = reverse l ++ l
  in toList . foldr fromCode empty . map code $ input

retrieve : Code -> List (Nat, Code) -> Maybe Nat
retrieve c = map fst . find (equals c . snd)

unelems : List Nat -> Nat
unelems = sum . zipWith (*) [1000, 100, 10, 1]

translate : List (Nat, Code) -> List String -> Maybe Nat
translate m = map unelems . traverse (`retrieve` m) . map code

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl08/input"
  -- Ex 1
  printLn . sum . map (countSimple . outValsLen) $ res

  -- Ex 2
  let dmaps = map (getDigitMap . uncurry (++)) res
  let sols = traverse (uncurry translate)
           . zip dmaps . map (forget . snd) $ res

  printLn $ sum <$> sols

