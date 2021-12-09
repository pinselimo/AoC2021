module Fensterl05.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

public export
data Vent : Type -> Type where
  MkVent : Num a => (a, a) -> (a, a) -> Vent a

export
Show a => Show (Vent a) where
  show (MkVent a b) = show a ++ " | " ++ show b

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkSep : Token
  TkArr : Token
  TkNumber : Nat -> Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match digits (TkNumber . cast)
        <|> match (exact ",") (const TkSep)
        <|> match (exact "->") (const TkArr)

grammarNewline : Grammar () Token True ()
grammarNewline = let
  getNewline : Token -> Maybe ()
  getNewline x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNewline

grammarNumber : Grammar () Token True Nat
grammarNumber = let
  getNat : Token -> Maybe Nat
  getNat x = case x of
                  TkNumber n => Just n
                  _ => Nothing
  in terminal "Number" getNat

grammarWhitespace : Grammar () Token True ()
grammarWhitespace = let
  getWS : Token -> Maybe ()
  getWS x = case x of
                 TkWhitespace => Just ()
                 _ => Nothing
  in terminal "Whitespace" getWS

grammarArrow : Grammar () Token True ()
grammarArrow = let
  getArr : Token -> Maybe ()
  getArr x = case x of
                  TkArr => Just ()
                  _ => Nothing
  in terminal "Arrow" getArr

grammarSep : Grammar () Token True ()
grammarSep = let
  getSep : Token -> Maybe ()
  getSep x = case x of
                  TkSep => Just ()
                  _ => Nothing
  in terminal "Sep" getSep

grammarCoords : Grammar () Token True (Nat -> (Nat, Nat))
grammarCoords = (,) <$> grammarNumber <* grammarSep

grammarVent : Grammar () Token True ((Nat, Nat) -> Vent Nat)
grammarVent = MkVent <$>
              (grammarCoords <*> grammarNumber) <* grammarWhitespace <* grammarArrow

grammarLine : Grammar () Token True (Vent Nat)
grammarLine = grammarVent <*>
              (grammarWhitespace 
            *> grammarCoords <*> grammarNumber <*
               grammarNewline)

grammarEmptyLine : Grammar () Token True (List ())
grammarEmptyLine = manyTill grammarNewline grammarWhitespace

export
grammar : Grammar () Token True (List1 (Vent Nat))
grammar = someTill (manyTill eof grammarEmptyLine) grammarLine

