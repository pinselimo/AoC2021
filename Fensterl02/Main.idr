module Fensterl02.Main

import Data.List1

import Common.Input
import Fensterl02.Parser

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

followCommands : List1 SubCommand -> (SubCommand -> a -> a) -> a -> a
followCommands cmds exec init = let
  orders : List1 (a -> a)
  orders = map exec cmds
  in foldl1By (\pos, order => order pos) ($init) orders

multiplyResult : Position -> Int
multiplyResult (Pos x y) = x * y

-- Ex 2

data State : Type where
  St : Int -> Int -> Int -> State

initState : State
initState = St 0 0 0

fromState : State -> Position
fromState (St x y _) = Pos x y

followCommand' : SubCommand -> State -> State
followCommand' cmd (St x y aim) = case cmd of
                                   (Forward n) => let n' = cast n
                                                  in St (x + n') (y + aim*n') aim
                                   (Down n) => St x y $ aim + cast n
                                   (Up n) => St x y $ aim - cast n

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl02/input"
  printLn $ length $ forget res

  printLn $ multiplyResult $ followCommands res followCommand initPos

  printLn $ multiplyResult $ fromState $ followCommands res followCommand' initState
