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

module Parser (
  parse
) where

import Token
import AST

-- A recursive-descent parser that transforms a stream of tokens into a syntax tree.
--
-- Grammar:
-- p0 ::= <p2> <p1>
-- p1 ::= '+' <p2> <p1>
--    ::= '-' <p2> <p1>
--    ::= e
-- p2 ::= <p4> <p3>
-- p3 ::= '*' <p4> <p3>
--    ::= '/' <p4> <p3>
--    ::= '%' <p4> <p3>
--    ::= '^' <p4> <p3>
--    ::= e
-- p4 ::= <p6> <p5>
-- p5 ::= 'min' <p6> <p5>
--    ::= 'max' <p6> <p5>
--    ::= e
-- p6 ::= '(' <p0> ')'
--    ::= <symbol>
--    ::= <number>

parse :: [Token] -> AST
parse inp =
    let (ast, rest) = p0 inp
    in
      case rest of
        [] -> ast
        (t : _) -> error $ lexeme t ++ ": expecting " ++ lexeme EOSToken

--
-- p0 ::= <p2> <p1>
--
p0 :: [Token] -> (AST, [Token])
p0 inp =
    let (l, rest) = p2 inp
    in p1 l rest

--
-- p1 ::= '+' <p2> <p1>
--    ::= '-' <p2> <p1>
--    ::= e
--
p1 :: AST -> [Token] -> (AST, [Token])
p1 l inp =
    case inp of
      (PlusToken : rest) ->
        let (r, _rest) = p2 rest
        in p1 (AddAST l r) _rest
      (MinusToken : rest) ->
        let (r, _rest) = p2 rest
        in p1 (SubtractAST l r) _rest
      _ -> (l, inp)

--
-- p2 ::= <p4> <p3>
--
p2 :: [Token] -> (AST, [Token])
p2 inp =
    let (l, rest) = p4 inp
    in p3 l rest

--
-- p3 ::= '*' <p4> <p3>
--    ::= '/' <p4> <p3>
--    ::= '%' <p4> <p3>
--    ::= '^' <p4> <p3>
--    ::= e
--
p3 :: AST -> [Token] -> (AST, [Token])
p3 l inp =
    case inp of
      (StarToken : rest) ->
        let (r, _rest) = p4 rest
        in p3 (MultiplyAST l r) _rest
      (SlashToken : rest) ->
        let (r, _rest) = p4 rest
        in p3 (DivideAST l r) _rest
      (PercentToken : rest) ->
        let (r, _rest) = p4 rest
        in p3 (ModuloAST l r) _rest
      (CaretToken : rest) ->
        let (r, _rest) = p4 rest
        in p3 (PowerAST l r) _rest
      _ -> (l, inp)

--
-- p4 ::= <p6> <p5>
--
p4 :: [Token] -> (AST, [Token])
p4 inp =
    let (l, rest) = p6 inp
    in p5 l rest

--
-- p5 ::= 'min' <p6> <p5>
--    ::= 'max' <p6> <p5>
--    ::= e
--
p5 :: AST -> [Token] -> (AST, [Token])
p5 l inp =
    case inp of
      (MinToken : rest) ->
        let (r, _rest) = p6 rest
        in p5 (MinAST l r) _rest
      (MaxToken : rest) ->
        let (r, _rest) = p6 rest
        in p5 (MaxAST l r) _rest
      _ -> (l, inp)

--
-- p6 ::= '(' <p0> ')'
--    ::= <symbol>
--    ::= <number>
--
p6 :: [Token] -> (AST, [Token])
p6 inp =
    case inp of
      (LeftParenToken : rest) ->
        let (ast, _rest) = p0 rest
        in (ast, confirm _rest RightParenToken)
      (SymbolToken lex : rest) -> (SymbolAST lex, rest)
      (NumberToken lex : rest) -> (NumberAST $ read lex, rest)
      _ -> unexpected $ if null inp then EOSToken else head inp
  where
    unexpected t = error $ lexeme t ++ ": expecting '(', <symbol> or <number>"

confirm :: [Token] -> Token -> [Token]
confirm inp token =
    case inp of
      (t : rest) -> if t == token then rest else unexpected t
      _ -> unexpected EOSToken
  where
    unexpected t = error $ lexeme t ++ ": expecting '" ++ lexeme token ++ "'"
