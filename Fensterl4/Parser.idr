module Fensterl4.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser
import Data.Nat
import Text.Quantity

import Fensterl4.Types

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkNumber : Nat -> Token
  TkSep : Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match digits (TkNumber . cast)
        <|> match (exact ",") (const TkSep)

grammarNewline : Grammar () Token True ()
grammarNewline = let
  getNewline : Token -> Maybe ()
  getNewline x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNewline

grammarNumber : Grammar () Token True Nat
grammarNumber = let
  getNumber : Token -> Maybe Nat
  getNumber x = case x of
                  TkNumber n => Just n
                  _ => Nothing
  in terminal "Number" getNumber

grammarWhitespace : Grammar () Token True ()
grammarWhitespace = let
  getWS : Token -> Maybe ()
  getWS x = case x of
                 TkWhitespace => Just ()
                 _ => Nothing
  in terminal "Whitespace" getWS

grammarSep : Grammar () Token True ()
grammarSep = let
  getSep : Token -> Maybe ()
  getSep x = case x of
                  TkSep => Just ()
                  _ => Nothing
  in terminal "Sep" getSep

grammarRandsLine : Grammar () Token True (a -> (List Nat, a))
grammarRandsLine = map (,) $ sepBy grammarSep grammarNumber <* grammarNewline

grammarBingoLine : Grammar () Token True (List Nat)
grammarBingoLine = many grammarWhitespace
                *> sepBy grammarWhitespace grammarNumber <*
                   grammarNewline

grammarEmptyLine : Grammar () Token True ()
grammarEmptyLine = map (const ()) $ manyTill grammarNewline grammarWhitespace

grammarBingo : Grammar () Token True (List1 (List Nat))
grammarBingo = someTill
              (eof <|> Delay grammarEmptyLine)
               grammarBingoLine

export
grammar : Grammar () Token True (List Nat, Maybe (List1 (Bingo 5 Nat)))
grammar = let conv = flip for $ gridFromList 5 . forget 
    in grammarRandsLine <* grammarEmptyLine
        <*> 
       conv <$> someTill (manyTill eof grammarEmptyLine) grammarBingo

