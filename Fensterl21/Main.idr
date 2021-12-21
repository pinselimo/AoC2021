module Fensterl21.Main

import Control.Monad.State
import Data.Nat
import Data.Stream
import Data.List -- NonEmpty

die : Stream Nat
die = cycle [1..100]

playerStep : Stream Nat -> Nat -> (Stream Nat, Nat)
playerStep ns pos = let
  pos' = (pos+sum (take 3 ns)) `mod` 10
  in (drop 3 ns,) $ if pos' == 0 then 10 else pos'

gameStep : Stream Nat -> (Nat, Nat) -> (Nat, Nat)
        -> (Stream Nat, (Nat, Nat), (Nat, Nat))
gameStep s (p1sc, p1pos) (p2sc, p2pos) = let
  (s', p1pos') = playerStep s p1pos
  (s'', p2pos') = playerStep s' p2pos
  in (s'', (p1sc+p1pos', p1pos'), (p2sc+p2pos', p2pos'))

game : Nat -> Nat -> Nat
game p1pos p2pos = game' 0 die
  where
    game' : Nat -> Stream Nat -> Nat
    game' n ns = go n ns (0, p1pos) (0, p2pos)
      where
        go : Nat -> Stream Nat -> (Nat, Nat) -> (Nat, Nat) -> Nat
        go n ns p1 p2 = let
            (ns', p1', p2') = gameStep ns p1 p2
            in  if fst p1' >= 1000
                   then (n*6+3) * fst p2
                   else if fst p2' >= 1000
                           then (n*6+6) * fst p1'
                           else go (S n) ns' p1' p2'

main : HasIO io => io ()
main = do
  printLn (game 7 5)

