module Fensterl16.Parser

import Data.String
import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkHex : String -> Token

toBin : String -> String
toBin = go . toUpper
  where
    go : String -> String
    go "0" = "0000"
    go "1" = "0001"
    go "2" = "0010"
    go "3" = "0011"
    go "4" = "0100"
    go "5" = "0101"
    go "6" = "0110"
    go "7" = "0111"
    go "8" = "1000"
    go "9" = "1001"
    go "A" = "1010"
    go "B" = "1011"
    go "C" = "1100"
    go "D" = "1101"
    go "E" = "1110"
    go "F" = "1111"
    go _   = "" -- remove faulty ones

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match hexDigit (TkHex)

grammarNewline : Grammar () Token True ()
grammarNewline = let
  getNewline : Token -> Maybe ()
  getNewline x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNewline

grammarHex : Grammar () Token True String
grammarHex = let
  getHex : Token -> Maybe String
  getHex x = case x of
                  TkHex n => Just n
                  _ => Nothing
  in terminal "Hex" getHex

grammarWhitespace : Grammar () Token True ()
grammarWhitespace = let
  getWS : Token -> Maybe ()
  getWS x = case x of
                 TkWhitespace => Just ()
                 _ => Nothing
  in terminal "Whitespace" getWS

grammarLine : Grammar () Token True (List1 String)
grammarLine = some grammarHex <* grammarNewline

grammarEmptyLine : Grammar () Token True ()
grammarEmptyLine = map (const ()) $ manyTill grammarNewline grammarWhitespace

export
grammar : Grammar () Token True String
grammar = (concat . map toBin) <$> grammarLine <* manyTill eof grammarEmptyLine
