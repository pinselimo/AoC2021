module Fensterl18.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

public export
data Snail : Type where
  Pair : Snail -> Snail -> Snail
  Numb : Nat -> Snail

export
Show Snail where
  show (Pair n m) = "["++show n++","++show m++"]"
  show (Numb n) = show n

export
Eq Snail where
  (Pair a b) == (Pair c d) = a == c && b == d
  (Numb n) == (Numb m) = n == m
  _ == _ = False

data Token = TkWhitespace
           | TkNewline
           | TkBracketsOpen
           | TkBracketsClose
           | TkNumber Nat
           | TkSep

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match digits (TkNumber . cast)
        <|> match (exact "[") (const TkBracketsOpen)
        <|> match (exact "]") (const TkBracketsClose)
        <|> match (exact ",") (const TkSep)

grNewline : Grammar () Token True ()
grNewline = let
  getNl : Token -> Maybe ()
  getNl x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNl

grWhitespace : Grammar () Token True ()
grWhitespace = let
  getWs : Token -> Maybe ()
  getWs x = case x of
                  TkWhitespace => Just ()
                  _ => Nothing
  in terminal "Whitespace" getWs

grNumber : Grammar () Token True Snail
grNumber = let
  getN : Token -> Maybe Snail
  getN x = case x of
                TkNumber n => Just $ Numb n
                _ => Nothing
  in terminal "Number" getN

grBrOpen : Grammar () Token True ()
grBrOpen = let
  getBr : Token -> Maybe ()
  getBr x = case x of
                  TkBracketsOpen => Just ()
                  _ => Nothing
  in terminal "[" getBr

grBrClose : Grammar () Token True ()
grBrClose = let
  getBr : Token -> Maybe ()
  getBr x = case x of
                  TkBracketsClose => Just ()
                  _ => Nothing
  in terminal "]" getBr

grSep : Grammar () Token True ()
grSep = let
  getSp : Token -> Maybe ()
  getSp x = case x of
                  TkSep => Just ()
                  _ => Nothing
  in terminal "," getSp

grEmptyLine : Grammar () Token True ()
grEmptyLine = const ()
          <$> manyTill grNewline grWhitespace

grammarSnail : Grammar () Token True Snail
grammarSnail = grBrOpen *> Pair 
           <$> (grammarSnail <|> grNumber) <* grSep
           <*> (grammarSnail <|> grNumber) <*  grBrClose

grammarLine : Grammar () Token True Snail
grammarLine = grammarSnail <* grNewline

export
grammar : Grammar () Token True (List1 Snail)
grammar = someTill (manyTill eof grEmptyLine) grammarLine

