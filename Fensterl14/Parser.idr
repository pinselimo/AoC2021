module Fensterl14.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser
import Data.SortedMap

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkUpper : String -> Token
  TkSep : Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match upper TkUpper
        <|> match (exact "->") (const TkSep)

grammarNewline : Grammar () Token True ()
grammarNewline = let
  getNewline : Token -> Maybe ()
  getNewline x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNewline

grammarUpper : Grammar () Token True String
grammarUpper = let
  getUpper : Token -> Maybe String
  getUpper x = case x of
                  TkUpper n => Just n
                  _ => Nothing
  in terminal "Element" getUpper

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
  in grammarWhitespace *> terminal "->" getSep <* grammarWhitespace

grammarPolyLine : Grammar () Token True (List1 String)
grammarPolyLine = some grammarUpper <* grammarNewline

grammarInsertionLine : Grammar () Token True ((String, String), String)
grammarInsertionLine = (,) <$> ((,) <$> grammarUpper <*> grammarUpper)
                   <*> (grammarSep *> grammarUpper <* grammarNewline)

grammarEmptyLine : Grammar () Token True ()
grammarEmptyLine = map (const ()) $ manyTill grammarNewline grammarWhitespace

export
grammar : Grammar () Token True (List1 String, SortedMap (String, String) String)
grammar = (,) <$> grammarPolyLine <* grammarEmptyLine <*> fromList
      <$> manyTill (manyTill eof grammarEmptyLine) grammarInsertionLine

