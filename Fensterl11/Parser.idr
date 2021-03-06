module Fensterl11.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkDigit : Nat -> Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match digit (TkDigit . cast)

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

grammarLine : Grammar () Token True (List1 Nat)
grammarLine = someTill grammarNewline grammarDigits

grammarEmptyLine : Grammar () Token True ()
grammarEmptyLine = map (const ()) $ manyTill grammarNewline grammarWhitespace

export
grammar : Grammar () Token True (List1 (List1 Nat))
grammar = someTill (manyTill eof grammarEmptyLine) grammarLine

