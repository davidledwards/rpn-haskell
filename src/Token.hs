-- Copyright 2016 David Edwards
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Token (
  Token(..),
  lexeme,
  charIsToken,
  charToToken,
  asToken
) where

class Tokenized t where
  lexeme :: t -> String

data Token =
    PlusToken
  | MinusToken
  | StarToken
  | SlashToken
  | PercentToken
  | CaretToken
  | LeftParenToken
  | RightParenToken
  | MinToken
  | MaxToken
  | SymbolToken String
  | NumberToken String
  | EOSToken
  deriving (Eq, Show)

instance Tokenized Token where
  lexeme PlusToken = "+"
  lexeme MinusToken = "-"
  lexeme StarToken = "*"
  lexeme SlashToken = "/"
  lexeme PercentToken = "%"
  lexeme CaretToken = "^"
  lexeme LeftParenToken = "("
  lexeme RightParenToken = ")"
  lexeme MinToken = "min"
  lexeme MaxToken = "max"
  lexeme EOSToken = "<EOS>"
  lexeme (SymbolToken lex) = lex
  lexeme (NumberToken lex) = lex

charIsToken :: Char -> Bool
charIsToken c = elem c ['+', '-', '*', '/', '%', '^', '(', ')']

charToToken :: Char -> Token
charToToken '+' = PlusToken
charToToken '-' = MinusToken
charToToken '*' = StarToken
charToToken '/' = SlashToken
charToToken '%' = PercentToken
charToToken '^' = CaretToken
charToToken '(' = LeftParenToken
charToToken ')' = RightParenToken

asToken :: String -> Token
asToken "min" = MinToken
asToken "max" = MaxToken
asToken lex = SymbolToken lex
