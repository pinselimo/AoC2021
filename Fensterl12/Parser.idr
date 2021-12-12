module Fensterl12.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkChars : String -> Token
  TkDel : Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match alphas TkChars
        <|> match (exact "-") (const TkDel)

grammarNewline : Grammar () Token True ()
grammarNewline = let
  getNewline : Token -> Maybe ()
  getNewline x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNewline

grammarChars : Grammar () Token True String
grammarChars = let
  getChars : Token -> Maybe String
  getChars x = case x of
                  TkChars cs => Just cs
                  _ => Nothing
  in terminal "Chars" getChars

grammarWhitespace : Grammar () Token True ()
grammarWhitespace = let
  getWS : Token -> Maybe ()
  getWS x = case x of
                 TkWhitespace => Just ()
                 _ => Nothing
  in terminal "Whitespace" getWS

grammarDel : Grammar () Token True ()
grammarDel = let
  getDel : Token -> Maybe ()
  getDel x = case x of
                  TkDel => Just ()
                  _ => Nothing
  in terminal "Delimiter" getDel

grammarLine : Grammar () Token True (String, String)
grammarLine = (,)
          <$> grammarChars <* grammarDel
          <*> grammarChars <* grammarNewline

grammarEmptyLine : Grammar () Token True ()
grammarEmptyLine = const ()
               <$> manyTill grammarNewline grammarWhitespace

export
grammar : Grammar () Token True (List1 (String, String))
grammar = someTill (manyTill eof grammarEmptyLine) grammarLine

