-- Inspired by this template:
-- https://github.com/jumper149/AoC2021/blob/main/00-template/Main.idr

import AoC.Input
import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

data Token : Type where
  TkNewline : Token

tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
      <|> ?tokenize

grammarNewline : Grammar () Token True ()
grammarNewline = let
  getNewline : Token -> Maybe ()
  getNewline x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNewline

grammar : Grammar () Token True ?fill
grammar = ?grammarize

main : HasIO io => io ()
main = pure ()
