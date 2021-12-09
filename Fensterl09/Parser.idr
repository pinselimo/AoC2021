module Fensterl9.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkDigit : Nat -> Token
  TkSep : Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match digit (TkDigit . cast)
        <|> match (exact ",") (const TkSep)

grammarNewline : Grammar () Token True ()
grammarNewline = let
  getNewline : Token -> Maybe ()
  getNewline x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNewline

grammarDigits : Grammar () Token True Nat
grammarDigits = let
  getDigit : Token -> Maybe Nat
  getDigit x = case x of
                  TkDigit n => Just n
                  _ => Nothing
  in terminal "Digit" getDigit

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

grammarLine : Grammar () Token True (List1 Nat)
grammarLine = someTill grammarNewline grammarDigits

grammarEmptyLine : Grammar () Token True ()
grammarEmptyLine = map (const ()) $ manyTill grammarNewline grammarWhitespace

export
grammar : Grammar () Token True (List1 (List1 Nat))
grammar = someTill (manyTill eof grammarEmptyLine) grammarLine

