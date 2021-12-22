module Fensterl22.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

public export
data State = On | Off
export
Show State where
  show On = "on"
  show Off = "off"
export
Eq State where
  On == On = True
  Off == Off = True
  _ == _ = False

public export
data Dir = X | Y | Z
export
Show Dir where
  show X = "x"
  show Y = "y"
  show Z = "z"

public export
Command : Type
Command = (Dir, (Int, Int))

data Token = TkNewline
           | TkWhitespace
           | TkCommand String
           | TkNumber Int
           | TkDirection String
           | TkDotDot
           | TkSep

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match space (const TkWhitespace)
        <|> match (oneOf ",=.") (const TkSep)
        <|> match (oneOf "xyz") TkDirection
        <|> match intLit (TkNumber . cast)
        <|> match (choice [exact "on", exact "off"]) TkCommand

grNewline : Grammar () Token True ()
grNewline = terminal "\\n" (\x => case x of
                                    TkNewline => Just ()
                                    _ => Nothing)

grWhitespace : Grammar () Token True ()
grWhitespace = terminal "<space>" (\x => case x of
                                           TkWhitespace => Just ()
                                           _ => Nothing)

grSep : Grammar () Token True ()
grSep = terminal "[sep]" (\x => case x of
                                   TkSep => Just ()
                                   _ => Nothing)

grState : Grammar () Token True State
grState = terminal "[cmd]" (\x => case x of
                                   TkCommand "on"  => Just On
                                   TkCommand "off" => Just Off
                                   _ => Nothing)

grDir : Grammar () Token True Dir
grDir = terminal "[dir]" (\x => case x of
                                   TkDirection "x" => Just X
                                   TkDirection "y" => Just Y
                                   TkDirection "z" => Just Z
                                   _ => Nothing)

grNum : Grammar () Token True Int
grNum = terminal "[int]" (\x => case x of
                                   TkNumber i => Just i
                                   _ => Nothing)

grEmptyLine : Grammar () Token True ()
grEmptyLine = manyTill grNewline grWhitespace <&> const ()

grRange : Grammar () Token True Command
grRange = do
  d <- grDir <* grSep
  s <- grNum <* grSep
  e <- grSep *> grNum
  pure (d, (s,e))

grLine : Grammar () Token True (State, (Command, Command, Command))
grLine = (,) <$> grState <* grWhitespace
     <*> [(x,y,z) | x <- grRange <* grSep
                  , y <- grRange <* grSep
                  , z <- grRange ] <* grNewline

export
grammar : Grammar () Token True (List1 (State, (Command, Command, Command)))
grammar = someTill (manyTill eof grEmptyLine) grLine

