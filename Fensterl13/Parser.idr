module Fensterl13.Parser

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser
import Text.Quantity

public export
data Direction = X | Y

export
Show Direction where
  show X = "x"
  show Y = "y"

data Token : Type where
  TkNewline : Token
  TkWhitespace : Token
  TkNumber : Integer -> Token
  TkDirection : String -> Token
  TkInstruction : Token
  TkSep : Token
  TkEq : Token

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match spaces (const TkWhitespace)
        <|> match digits (TkNumber . cast)
        <|> match (exact ",") (const TkSep)
        <|> match (exact "=") (const TkEq)
        <|> match (exact "fold along") (const TkInstruction)
        <|> match (exact "x" <|> exact "y") TkDirection

grammarNewline : Grammar () Token True ()
grammarNewline = let
  getNewline : Token -> Maybe ()
  getNewline x = case x of
                  TkNewline => Just ()
                  _ => Nothing
  in terminal "Newline" getNewline

grammarNumber : Grammar () Token True Integer
grammarNumber = let
  getNumber : Token -> Maybe Integer
  getNumber x = case x of
                  TkNumber n => Just n
                  _ => Nothing
  in terminal "Number" getNumber

grammarWhitespace : Grammar () Token True ()
grammarWhitespace = let
  getWS : Token -> Maybe ()
  getWS x = case x of
                 TkWhitespace => Just ()
                 _ => Nothing
  in terminal "Whitespace" getWS

grammarEq : Grammar () Token True ()
grammarEq = let
  getEq : Token -> Maybe ()
  getEq x = case x of
                  TkEq => Just ()
                  _ => Nothing
  in terminal "=" getEq

grammarSep : Grammar () Token True ()
grammarSep = let
  getSep : Token -> Maybe ()
  getSep x = case x of
                  TkSep => Just ()
                  _ => Nothing
  in terminal "Sep" getSep

grammarInstr : Grammar () Token True ()
grammarInstr = let
  getInstr : Token -> Maybe ()
  getInstr x = case x of
                  TkInstruction => Just ()
                  _ => Nothing
  in terminal "Instruction" getInstr

grammarDirection : Grammar () Token True Direction
grammarDirection = let
  getDir : Token -> Maybe Direction
  getDir x = case x of
                  TkDirection "x" => Just X
                  TkDirection "y" => Just Y
                  _ => Nothing
  in terminal "Directoin" getDir

grammarRandsLine : Grammar () Token True (Integer, Integer)
grammarRandsLine = (,) <$> grammarNumber <* grammarSep 
                       <*> grammarNumber <* grammarNewline

grammarInstrLine : Grammar () Token True (Direction, Integer)
grammarInstrLine = (,) <$> (grammarInstr *> grammarWhitespace *> grammarDirection)
                       <*> (grammarEq *> grammarNumber <* grammarNewline)

grammarEmptyLine : Grammar () Token True ()
grammarEmptyLine = map (const ()) $ manyTill grammarNewline grammarWhitespace

grammarRands : Grammar () Token True (List1 (Integer, Integer))
grammarRands = someTill grammarEmptyLine grammarRandsLine

export
grammar : Grammar () Token True (List1 (Integer, Integer), (List1 (Direction, Integer)))
grammar = (,) <$> grammarRands
              <*> someTill (manyTill eof grammarEmptyLine) grammarInstrLine

