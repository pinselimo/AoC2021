module Fensterl6.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

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

grammarLine : Grammar () Token True (List1 Nat)
grammarLine = sepBy1 grammarSep grammarNumber <* grammarNewline

grammarEmptyLine : Grammar () Token True ()
grammarEmptyLine = map (const ()) $ manyTill grammarNewline grammarWhitespace

export
grammar : Grammar () Token True (List1 Nat)
grammar = grammarLine <* manyTill eof grammarEmptyLine

