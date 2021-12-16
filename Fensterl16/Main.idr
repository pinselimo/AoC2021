module Fensterl16.Main

import Data.Nat
import Data.List
import Data.List1
import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

import Debug.Trace

import Common.Input
import Fensterl16.Parser
import Fensterl03.Binaries

data Bit = O | I

-- from Fensterl03.Main
toBin : List Bit -> Bin
toBin = toBin' . reverse
  where
    toBin' : List Bit -> Bin
    toBin' [] = Z
    toBin' (I::r) = B1 $ toBin' r
    toBin' (O::r) = B0 $ toBin' r

bitsInt : List Bit -> List Integer
bitsInt = map int
  where int : Bit -> Integer
        int I = 1
        int O = 0

fromBits : List Bit -> Nat
fromBits = integerToNat . foldl (\acc, d => d + 2 * acc) 0 . bitsInt

data TkBit : Type where
  TkOne : String -> TkBit
  TkZero : String -> TkBit

Show TkBit where
  show (TkOne _) = "Tk I"
  show (TkZero _) = "Tk O"

bitTk : Bit -> TkBit
bitTk I = TkOne "1"
bitTk O = TkZero "0"

Show Bit where
  show I = "I"
  show O = "O"

data Packet = Operator Nat Nat (List Packet)
            | Literal Nat Nat

Show Packet where
  show (Operator v i ps) = "OP " ++ show v ++ " " ++ show i ++ show ps
  show (Literal v i) = "LIT " ++ show v ++ " - " ++ show i

binTokenizer : Tokenizer TkBit
binTokenizer = match (exact "1") TkOne
           <|> match (exact "0") TkZero

grZero : Grammar () TkBit True Bit
grZero = let
  zero : TkBit -> Maybe Bit
  zero (TkZero _) = pure O
  zero _ = Nothing
  in terminal "Bit O" zero

grOne : Grammar () TkBit True Bit
grOne = let
  one : TkBit -> Maybe Bit
  one (TkOne _) = pure I
  one _ = Nothing
  in terminal "Bit I" one

grBit : Grammar () TkBit True Bit
grBit = grOne <|> grZero

grLiteralN : Grammar () TkBit True (List Bit)
grLiteralN = do
  v <- concat <$> many going
  l <- last
  pure $ v ++ l
  where
    last : Grammar () TkBit True (List Bit)
    last = do
      _ <- grZero
      count (atMost 4) grBit
    going : Grammar () TkBit True (List Bit)
    going = do
      _ <- grOne
      count (atMost 4) grBit

grLiteral : Grammar () TkBit True Packet
grLiteral = do
  v <- fromBits <$> count (atMost 3) grBit
  _ <- grOne *> grZero *> grZero
  n <- fromBits <$> grLiteralN
  pure $ Literal v n

grPacket : Grammar () TkBit True Packet

grOperator : Grammar () TkBit True Packet
grOperator = do
  v  <- fromBits <$> count (atMost 3) grBit
  id <- fromBits <$> count (atMost 3) grBit
  ps <- this <|> that
  pure $ Operator v id ps
    where
      that : Grammar () TkBit True (List Packet)
      that = do
        _ <- grOne
        n <- fromBits <$> count (atMost 11) grBit
        count (atMost n) grPacket

      this : Grammar () TkBit True (List Packet)
      this = do
        _ <- grZero
        len <- fromBits <$> count (atMost 15) grBit
        subData <- map irrelevantBounds <$> count (atMost len) grBit
        pure $ (case parse (mapToken bitTk $ many grPacket) subData of
             Left err => ?hole
             Right (ps, _) => ps)


grPacket = (try grLiteral) <|> grOperator

parsePacket : HasIO io => String -> io Packet
parsePacket inp = do 
  tok <- handleEitherErr show (eitherLex binTokenizer inp)
  par <- handleEitherErr extractParsingError . parse (grPacket <* many grZero) $ tok
  pure . Builtin.fst $ par

-- Ex 1:
versionSum : Packet -> Nat
versionSum (Operator v _ ps) = v + (sum $ map versionSum ps)
versionSum (Literal v _) = v

-- Ex 2:
totalPacket : Packet -> Nat
totalPacket (Literal v n) = n
totalPacket (Operator v id ps) = case id of
    0 => sum $ map totalPacket ps
    1 => product $ map totalPacket ps
    2 => foldr min 999999999 $ map totalPacket ps
    3 => foldr max 0 $ map totalPacket ps
    5 => case ps of
              [] => 0
              (_::[]) => 0
              (h::x::xs) => if totalPacket h > totalPacket (last (x:::xs))
                               then 1 else 0
    6 => case ps of
              [] => 0
              (_::[]) => 0
              (h::x::xs) => if totalPacket h < totalPacket (last (x:::xs))
                               then 1 else 0
    7 => case ps of
              [] => 0
              (_::[]) => 0
              (h::x::xs) => if totalPacket h == totalPacket (last (x:::xs))
                               then 1 else 0
    _ => 0

main : HasIO io => io ()
main = do
  inp <- Input.readInput tokenizer grammar "Fensterl16/input"
  printLn inp
  --packet <- parsePacket "00111000000000000110111101000101001010010001001000000000"

  packet <- parsePacket inp
  printLn $ versionSum packet
  printLn $ totalPacket packet

