module Token (
  Token(..),
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
