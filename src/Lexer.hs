module Lexer (
  lexer
) where

import Token
import Data.Char (isSpace, isDigit)

lexer :: String -> [Token]
lexer inp = tokens inp
  where
    tokens :: String -> [Token]
    tokens inp = case tokenize inp of
      (EOSToken, _) -> []
      (token, rest) -> token : tokens rest

tokenize :: String -> (Token, String)
tokenize [] = (EOSToken, [])
tokenize (c : rest)
    | isSpace c = tokenize rest
    | charIsToken c = (charToToken c, rest)
    | isDigit c = readNumber rest [c]
    | isLetter c = readSymbol rest [c]
    | otherwise = error $ c : ": unrecognized character"

readNumber :: String -> String -> (Token, String)
readNumber [] lex = (verifyNumber lex, [])
readNumber (c : rest) lex
    | c == '.' =
      if '.' `elem` lex
        then error $ lex ++ ".: malformed number"
        else readNumber rest $ lex ++ ['.']
    | isDigit c = readNumber rest $ lex ++ [c]
    | otherwise = (verifyNumber lex, c : rest)

verifyNumber :: String -> Token
verifyNumber lex
    | last lex == '.' = error $ lex ++ ": malformed number"
    | otherwise = NumberToken lex

readSymbol :: String -> String -> (Token, String)
readSymbol [] lex = (asToken lex, [])
readSymbol (c : rest) lex
    | isLetter c = readSymbol rest $ lex ++ [c]
    | otherwise = (asToken lex, rest)

isLetter :: Char -> Bool
isLetter c = c `elem` ['A'..'Z'] ++ ['a'..'z']
