module Fensterl01.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkNumber  : Nat -> Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match digits (TkNumber . cast)

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

grammarLine : Grammar () Token True Nat
grammarLine = grammarNumber <* grammarNewline

grammarWhitespace : Grammar () Token True ()
grammarWhitespace = let
  getWS : Token -> Maybe ()
  getWS x = case x of
                 TkWhitespace => Just ()
                 _ => Nothing
  in terminal "Whitespace" getWS

grammarEmptyLine : Grammar () Token True (List ())
grammarEmptyLine = manyTill grammarNewline grammarWhitespace

export
grammar : Grammar () Token True (List1 Nat)
grammar = someTill (manyTill eof grammarEmptyLine) grammarLine
