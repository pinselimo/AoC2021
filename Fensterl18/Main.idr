module Fensterl18.Main

import Data.List
import Data.List1
import Data.Nat
import Data.Maybe

import Debug.Trace

import Common.Input
import Fensterl18.Parser

data Explosion = Explode Nat Nat
               | AddLeft Nat Snail
               | AddRight Snail Nat
               | Changed Snail
               | Same

addRight : Nat -> Snail -> Snail
addRight n (Numb m) = Numb (n + m)
addRight n (Pair l r) = Pair l (addRight n r)

addLeft : Nat -> Snail -> Snail
addLeft n (Numb m) = Numb (n + m)
addLeft n (Pair l r) = Pair (addLeft n l) r

explode : Snail -> Maybe Snail
explode s = case explode' 0 s of
                 AddLeft _ s'  => Just s'
                 AddRight s' _ => Just s'
                 Explode _ _   => Nothing -- impossible
                 Changed s'    => Just s'
                 Same          => Nothing
  where
    explode' : Nat -> Snail -> Explosion
    explode' n (Pair (Numb a) (Numb b)) = if n >= 4 then Explode a b else Same
    explode' n (Pair a b) = case explode' (S n) a of
              Explode aa ab => AddLeft aa (Pair (Numb 0) (addLeft ab b))
              AddLeft n a'  => AddLeft n (Pair a' b)
              AddRight a' n => Changed (Pair a' (addLeft n b))
              Changed a'    => Changed (Pair a' b)
              Same => case explode' (S n) b of
                     Explode ba bb => AddRight (Pair (addRight ba a) (Numb 0)) bb
                     AddRight b' n => AddRight (Pair a b') n
                     AddLeft n b'  => Changed (Pair (addRight n a) b')
                     Changed b'    => Changed (Pair a b')
                     Same => Same
    explode' _ s = Same

split : Snail -> Either Snail Snail
split (Pair a b) = case (split a, split b) of
                        (Left a', _) => Left (Pair a' b)
                        (_, Left b') => Left (Pair a b')
                        _ => Right (Pair a b)
split (Numb n) = if n >= 10 
                    then let (n', r) = divmodNatNZ n 2 SIsNonZero 
                         in Left $ Pair (Numb n') (Numb $ r + n')
                    else Right (Numb n)

reduce : Snail -> Snail
reduce s = case explode s of
             Just s' => reduce s'
             Nothing => case split s of
                             Right _ => s
                             Left s' => reduce s'

addSnails : Snail -> Snail -> Snail
addSnails a = reduce . Pair a

magnitude : Snail -> Nat
magnitude (Pair a b) = 3*magnitude a + 2*magnitude b
magnitude (Numb n) = n

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl18/input"
  printLn $ magnitude $ foldl1By addSnails id res -- Ex 1

  -- Ex 2
  let largest : List Nat
      largest = reverse . sort
              . map (\x => fromMaybe 0 . head' . reverse . sort 
                         . map (magnitude . addSnails x) . filter (/=x) 
                         $ forget res
                         )
              $ forget res

  printLn $ head' largest

