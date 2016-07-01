module AST (
  AST(..),
  formatAST
) where

data AST =
    SymbolAST { name :: String }
  | NumberAST { value :: Double }
  | AddAST { l :: AST, r :: AST }
  | SubtractAST { l :: AST, r :: AST }
  | MultiplyAST { l :: AST, r :: AST }
  | DivideAST { l :: AST, r :: AST }
  | ModuloAST { l :: AST, r :: AST }
  | PowerAST { base :: AST, exp :: AST }
  | MinAST { l :: AST, r :: AST }
  | MaxAST { l :: AST, r :: AST }
  deriving (Eq, Show)

formatAST :: AST -> String
formatAST ast =
    format ast 0
  where
    format :: AST -> Int -> String
    format ast depth =
      replicate (depth * 2) ' ' ++
        case ast of SymbolAST name -> "Symbol(" ++ name ++ ")\n"
                    NumberAST value -> "Number(" ++ show value ++ ")\n"
                    AddAST l r -> "Add\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
                    SubtractAST l r -> "Subtract\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
                    MultiplyAST l r -> "Multiply\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
                    DivideAST l r -> "Divide\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
                    ModuloAST l r -> "Modulo\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
                    PowerAST base exp -> "Power\n" ++ (format base $ depth + 1) ++ (format exp $ depth + 1)
                    MinAST l r -> "Min\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
                    MaxAST l r -> "Max\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
