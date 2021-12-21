module Fensterl20.Main

import Data.List
import Data.List1
import Data.Maybe
import Data.Vect
import Data.String

import Common.Input
import Common.Comonad
import Common.Comonad.Store
import Fensterl20.Parser

Coord : Type
Coord = (Int, Int)

Img : Type
Img = Store Coord Pixel

infix 6 !!
(!!) : List a -> Int -> Maybe a
l !! i = if i < 0 then Nothing else index' l <$> natToFin (cast i) (length l)

fromList : List (List Pixel) -> Img
fromList ll = MkStore (\(x,y) => (ll !! x) >>= (!!y)) (0, 0)
          <&> fromMaybe Blank

dimNeighbors : Int -> List Int
dimNeighbors a = [a-1..a+1]

neighbors : Coord -> List Coord
neighbors (x, y) = [(x',y')
  | x' <- dimNeighbors x
  , y' <- dimNeighbors y
  ]

pixInt : Pixel -> Integer
pixInt Blank = 0
pixInt Full = 1

nbrsToFin : List Pixel -> Maybe (Fin 512)
nbrsToFin = (`natToFin` 512) . integerToNat . foldl (\acc, d => d + 2 * acc) 0 . map pixInt

step : Maybe (Vect 512 Pixel) -> Img -> Pixel
step enh img = let
  ns = (`peek` img) <$> neighbors (pos img)
  in fromMaybe Blank $ index <$> (nbrsToFin ns) <*> enh

display : Int -> Int -> Img -> String
display s e img = unlines [ unwords [ show (peek (x,y) img) | y <- [s..e] ]
                          | x <- [s..e]
                          ]

collect : Int -> Int -> Img -> Integer
collect s e img = sum [ pixInt (peek (x,y) img)
                      | x <- [s..e]
                      , y <- [s..e]
                      ]

main : HasIO io => io ()
main = do
  (enh, img) <- Input.readInput tokenizer grammar "Fensterl20/input"
  let img' = extend (step enh) $ extend (step enh) $ fromList img
  printLn $ collect (-10) (cast $ length img+10) img'
