module Fensterl3.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

public export
data Bit : Type where
  O : Bit
  Z : Bit

export
Show Bit where
  show O = "1"
  show Z = "0"

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkBit : Bit -> Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match (exact "1") (const $ TkBit O)
        <|> match (exact "0") (const $ TkBit Z)

grammarNewline : Grammar () Token True ()
grammarNewline = let
  getNewline : Token -> Maybe ()
  getNewline x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNewline

grammarBit : Grammar () Token True Bit
grammarBit = let
  getBit : Token -> Maybe Bit
  getBit x = case x of
                  TkBit b => Just b
                  _ => Nothing
  in terminal "Bit" getBit

grammarWhitespace : Grammar () Token True ()
grammarWhitespace = let
  getWS : Token -> Maybe ()
  getWS x = case x of
                 TkWhitespace => Just ()
                 _ => Nothing
  in terminal "Whitespace" getWS

grammarLine : Grammar () Token True (List Bit)
grammarLine = many grammarBit <* grammarNewline

grammarEmptyLine : Grammar () Token True (List ())
grammarEmptyLine = manyTill grammarNewline grammarWhitespace

export
grammar : Grammar () Token True (List1 (List Bit))
grammar = someTill (manyTill eof grammarEmptyLine) grammarLine

