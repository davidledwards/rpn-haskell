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
    format ast depth =
      replicate (depth * 2) ' ' ++
        case ast of
          SymbolAST name -> "Symbol(" ++ name ++ ")\n"
          NumberAST value -> "Number(" ++ show value ++ ")\n"
          AddAST l r -> "Add\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
          SubtractAST l r -> "Subtract\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
          MultiplyAST l r -> "Multiply\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
          DivideAST l r -> "Divide\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
          ModuloAST l r -> "Modulo\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
          PowerAST base exp -> "Power\n" ++ (format base $ depth + 1) ++ (format exp $ depth + 1)
          MinAST l r -> "Min\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
          MaxAST l r -> "Max\n" ++ (format l $ depth + 1) ++ (format r $ depth + 1)
