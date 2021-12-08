module Fensterl8.Main

import Data.List1

import AoC.Input
import Fensterl8.Parser

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

main : HasIO io => io ()
main = do
  res <- Input.readInput tokenizer grammar "Fensterl8/input"

  printLn . sum . map (countSimple . outValsLen) $ res

