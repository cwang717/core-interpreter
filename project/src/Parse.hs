-- | The parser goes here

module Parse where

import Prelude hiding (id)

import Types

import qualified Data.HashMap.Lazy as M

import Data.Functor.Identity
import Control.Monad
import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Parser
--- ----------

-- Pretty parser type
type Parser = ParsecT String () Identity

--- ### Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

id :: Parser Name
id = do first <- oneOf ['a' .. 'z']
        rest <- many (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'_")
        spaces
        return $ first:rest


  
eInt :: Parser Expr
eInt = do i <- int
          return $ ENum i

eVar :: Parser Expr
eVar = do name <- id
          return $ EVar name
          
expr :: Parser Expr
expr = orExpr

orExpr :: Parser Expr
orExpr = do
  left <- andExpr
  rest <- many (do _ <- symbol "|"
                   right <- andExpr
                   return right)
  return $ foldl (\l r -> EAp (EAp (EVar "|") l) r) left rest

andExpr :: Parser Expr
andExpr = do
  left <- eqExpr
  rest <- many (do _ <- symbol "&"
                   right <- eqExpr
                   return right)
  return $ foldl (\l r -> EAp (EAp (EVar "&") l) r) left rest

eqExpr :: Parser Expr
eqExpr = do
  left <- relExpr
  rest <- many (do op <- (symbol "==" <|> symbol "~=")
                   right <- relExpr
                   return (op, right))
  return $ foldl (\l (op, r) -> EAp (EAp (EVar op) l) r) left rest

relExpr :: Parser Expr
relExpr = do
  left <- addExpr
  rest <- many (do op <- (try (symbol "<=") <|> try (symbol ">=") <|> symbol "<" <|> symbol ">")
                   right <- addExpr
                   return (op, right))
  return $ foldl (\l (op, r) -> EAp (EAp (EVar op) l) r) left rest

addExpr :: Parser Expr
addExpr = do
  left <- mulExpr
  rest <- many (do op <- (symbol "+" <|> symbol "-")
                   right <- mulExpr
                   return (op, right))
  return $ foldl (\l (op, r) -> EAp (EAp (EVar op) l) r) left rest

mulExpr :: Parser Expr
mulExpr = do
  left <- atom
  rest <- many (do op <- (symbol "*" <|> symbol "/")
                   right <- atom
                   return (op, right))
  return $ foldl (\l (op, r) -> EAp (EAp (EVar op) l) r) left rest

atom :: Parser Expr
atom = eInt <|> eVar <|> parenExpr

parenExpr :: Parser Expr
parenExpr = do
  _ <- symbol "("
  e <- expr
  _ <- symbol ")"
  return e

decl :: Parser Decl
decl = do name <- id
          params <- many id
          _ <- symbol "="
          body <- expr
          return (name,params,body)

core :: Parser Core
core = do decls <- decl `sepBy` (symbol ";")
          return $ M.fromList [(n,v) | v@(n,_,_) <- decls]

parseCore :: String -> Either ParseError Core
parseCore text = parse core "Core" text
