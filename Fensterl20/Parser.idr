module Fensterl20.Parser

import Data.Vect
import Data.List

import Text.Lexer
import Text.Lexer.Tokenizer
import Text.Parser

public export
data Pixel = Full | Blank
export
Show Pixel where
  show Full = "#"
  show Blank = "."

data Token = TkNewline
           | TkWhitespace
           | TkFull
           | TkBlank

export
tokenizer : Tokenizer Token
tokenizer = match newline (const TkNewline)
        <|> match space (const TkWhitespace)
        <|> match (exact "#") (const TkFull)
        <|> match (exact ".") (const TkBlank)

grNewline : Grammar () Token True ()
grNewline = terminal "\\n" (\x => case x of
                                    TkNewline => Just ()
                                    _ => Nothing)

grWhitespace : Grammar () Token True ()
grWhitespace = terminal "<space>" (\x => case x of
                                           TkWhitespace => Just ()
                                           _ => Nothing)

grPixel : Grammar () Token True Pixel
grPixel = terminal "[pix]" (\x => case x of
                                   TkBlank => Just Blank
                                   TkFull  => Just Full
                                   _ => Nothing)

grEmptyLine : Grammar () Token True ()
grEmptyLine = manyTill grNewline grWhitespace <&> const ()

grEnhancement : Grammar () Token True (Maybe (Vect 512 Pixel))
grEnhancement = (toVect 512 . take 512) <$> (manyTill grNewline grPixel)

grLine : Grammar () Token True (List Pixel)
grLine = manyTill grNewline grPixel

export
grammar : Grammar () Token True (Maybe (Vect 512 Pixel), List (List Pixel))
grammar = do
  enh <- grEnhancement <* grEmptyLine
  img <- manyTill (manyTill eof grEmptyLine) grLine
  pure $ (enh, img)

