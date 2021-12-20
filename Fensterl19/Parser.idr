module Fensterl19.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

data Token = TkNewline
           | TkWhitespace
           | TkNumber Nat
           | TkCoordinate Int
           | TkSep
           | TkDashes
           | TkScanner

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match (surround space space digit) (TkNumber . cast)
        <|> match spaces  (const TkWhitespace)
        <|> match (exact ",") (const TkSep)
        <|> match (exact "---") (const TkDashes)
        <|> match (approx "scanner") (const TkScanner)
        <|> match intLit (TkCoordinate . cast)

grNewline : Grammar () Token True ()
grNewline = terminal "\\n" (\x => case x of
                                    TkNewline => Just ()
                                    _ => Nothing)

grWhitespace : Grammar () Token True ()
grWhitespace = terminal "<space>" (\x => case x of
                                           TkWhitespace => Just ()
                                           _ => Nothing)

grNum : Grammar () Token True Nat
grNum = terminal "Num" (\x => case x of
                                   TkNumber n => Just n
                                   _ => Nothing)

grCoord : Grammar () Token True Int
grCoord = terminal "Int" (\x => case x of
                                   TkCoordinate n => Just n
                                   _ => Nothing)

grSep : Grammar () Token True ()
grSep = terminal "," (\x => case x of
                              TkSep => Just ()
                              _ => Nothing)

grDashes : Grammar () Token True ()
grDashes = terminal "---" (\x => case x of
                                   TkDashes => Just ()
                                   _ => Nothing)

grScanner : Grammar () Token True ()
grScanner = terminal "scanner" (\x => case x of
                                        TkScanner => Just ()
                                        _ => Nothing)

grEmptyLine : Grammar () Token True ()
grEmptyLine = manyTill grNewline grWhitespace <&> const ()

grCoordinates : Grammar () Token True (Int, Int, Int)
grCoordinates = do
  a <- grCoord <* grSep
  b <- grCoord <* grSep
  c <- grCoord <* grNewline
  pure (a, b, c)

grammarScan : Grammar () Token True (Nat, List (Int, Int, Int))
grammarScan = do
  n <- grDashes *> grWhitespace *> grScanner *> grNum <* grDashes <* grNewline
  cs <- manyTill grEmptyLine grCoordinates
  pure (n, cs)

export
grammar : Grammar () Token True (List1 (Nat, List (Int, Int, Int)))
grammar = someTill (manyTill eof grEmptyLine) grammarScan

