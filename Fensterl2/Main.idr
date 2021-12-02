module Fensterl2.Main

import AoC.Input
import Fensterl2.Parser

import Data.List1

data Position : Type where
  Pos : Int -> Int -> Position -- not sure if it can be negative

initPos : Position
initPos = Pos 0 0

Show Position where
  show (Pos x y) = show (x,y) 

-- Ex 1
followCommand : SubCommand -> Position -> Position
followCommand cmd (Pos x y) = case cmd of
                                   (Forward n) => Pos (x + cast n) y
                                   (Down n) => Pos x $ y + cast n
                                   (Up n) => Pos x $ y - cast n

followCommands : List1 SubCommand -> Position -> Position
followCommands cmds init = let
  orders : List1 (Position -> Position)
  orders = map followCommand cmds
  in foldl1By (\pos, order => order pos) ($init) orders

multiplyResult : Position -> Int
multiplyResult (Pos x y) = x * y

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl2/input"
  printLn $ length $ forget res

  printLn $ multiplyResult $ followCommands res initPos
