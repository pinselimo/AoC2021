-- Inspired by this template:
-- https://github.com/jumper149/AoC2021/blob/main/00-template/Main.idr
module Common.Input

import Data.List1
import Text.Lexer.Tokenizer
import Text.Parser
import System.File.ReadWrite

export
eitherLex : Tokenizer a -> String -> 
            Either (StopReason, (Int, (Int, String))) (List (WithBounds a))
eitherLex tok inp = let
  (a, b) = lex tok inp
  in case b of
          (EndInput, _) => Right a
          _             => Left  b

export
handleEitherErr : HasIO io => (a -> String) -> Either a b -> io b
handleEitherErr conv arg = case arg of
      Left err  => putStrLn ("Error: " ++ conv err) >> pure ?error
      Right res => pure res

export
extractParsingError : List1 (ParsingError tok) -> String
extractParsingError = extract . head
  where
    extract : ParsingError tok -> String
    extract (Error msg Nothing) = msg
    extract (Error msg (Just bounds)) = msg ++ " at lines " 
                                            ++ (show $ startLine bounds) ++ ":" ++ (show $ endLine bounds)
                                            ++ " in columns "
                                            ++ (show $ startCol bounds) ++ ":" ++ (show $ endCol bounds)
export
readInput : HasIO io =>
            Tokenizer b -> Grammar () b True a -> String -> io a
readInput tokenizer grammar filename =
  readFile filename >>= handleEitherErr show
  >>= handleEitherErr show . eitherLex tokenizer
  >>= handleEitherErr extractParsingError . parse grammar
  >>= pure . Builtin.fst

