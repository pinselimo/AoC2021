module Fensterl02.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

public export
data SubCommand : Type where
  Forward : Nat -> SubCommand
  Down : Nat -> SubCommand
  Up : Nat -> SubCommand

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkCommand : (Nat -> SubCommand) -> Token
  TkNumber : Nat -> Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match (exact "forward") (const $ TkCommand Forward)
        <|> match (exact "down") (const $ TkCommand Down)
        <|> match (exact "up") (const $ TkCommand Up)
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

grammarCommand : Grammar () Token True (Nat -> SubCommand)
grammarCommand = let
  getCommand : Token -> Maybe (Nat -> SubCommand)
  getCommand x = case x of
                         TkCommand s => Just s
                         _ => Nothing
  in terminal "Command" getCommand

grammarWhitespace : Grammar () Token True ()
grammarWhitespace = let
  getWS : Token -> Maybe ()
  getWS x = case x of
                 TkWhitespace => Just ()
                 _ => Nothing
  in terminal "Whitespace" getWS

grammarLine : Grammar () Token True SubCommand
grammarLine = grammarCommand <*>
              (grammarWhitespace *> grammarNumber <* grammarNewline)

grammarEmptyLine : Grammar () Token True (List ())
grammarEmptyLine = manyTill grammarNewline grammarWhitespace

export
grammar : Grammar () Token True (List1 SubCommand)
grammar = someTill (manyTill eof grammarEmptyLine) grammarLine

