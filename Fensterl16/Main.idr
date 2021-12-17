module Fensterl16.Main

import Data.Nat
import Data.List1
import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

import Common.Input
import Fensterl16.Parser

data Bit = O | I

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

bitTk : Bit -> TkBit
bitTk I = TkOne "1"
bitTk O = TkZero "0"

data Packet = Operator Nat Nat (List Packet)
            | Literal Nat Nat

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
  v <- concat <$> many (grOne *> count (atMost 4) grBit)
  l <- grZero *> count (atMost 4) grBit
  pure $ v ++ l

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
      that = grOne *> do
        n <- fromBits <$> count (atMost 11) grBit
        count (atMost n) grPacket

      this : Grammar () TkBit True (List Packet)
      this = grZero *> do
        len <- fromBits <$> count (atMost 15) grBit
        subData <- map irrelevantBounds <$> count (atMost len) grBit
        pure $ (case parse (mapToken bitTk $ many grPacket) subData of
             Left err => []
             Right (ps, _) => ps)

grPacket = try grLiteral <|> grOperator

parsePacket : HasIO io => String -> io Packet
parsePacket inp = handleEitherErr show (eitherLex binTokenizer inp)
  >>= handleEitherErr extractParsingError . parse (grPacket <* many grZero)
  >>= pure . Builtin.fst

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
  --printLn inp

  packet <- parsePacket inp
  printLn $ versionSum packet
  printLn $ totalPacket packet

