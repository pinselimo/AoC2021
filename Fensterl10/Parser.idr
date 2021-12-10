module Fensterl10.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser
import Data.Nat
import Text.Quantity

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkParens : String -> Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match (oneOf "<{[()]}>") TkParens

grammarNewline : Grammar () Token True ()
grammarNewline = let
  getNewline : Token -> Maybe ()
  getNewline x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNewline

grammarWhitespace : Grammar () Token True ()
grammarWhitespace = let
  getWS : Token -> Maybe ()
  getWS x = case x of
                 TkWhitespace => Just ()
                 _ => Nothing
  in terminal "Whitespace" getWS

grammarParens : Grammar () Token True String
grammarParens = let
  getPs : Token -> Maybe String
  getPs x = case x of
                 TkParens s => Just s
                 _ => Nothing
  in terminal "Parens" getPs

grammarEmptyLine : Grammar () Token True ()
grammarEmptyLine = map (const ()) $ manyTill grammarNewline grammarWhitespace

grammarValidLine : Grammar () Token True (List String)
grammarValidLine = many grammarParens <* grammarNewline

export
grammar : Grammar () Token True (List1 (List String))
grammar = someTill (manyTill eof grammarEmptyLine) grammarValidLine

