module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr
import Data.Char
import Debug.Trace

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    return x = Parser $ \s -> Just (x, s)
    mp >>= f = Parser $ \s -> case parse mp s of 
        Nothing -> Nothing
        Just (val, rest) -> parse (f val) rest

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

instance Alternative Parser where
  empty = failParser
  p1 <|> p2 = Parser $ \s ->
    case parse p1 s of
        Nothing -> parse p2 s
        Just (x) -> Just (x)

--- type declaration over ---

-- TODO 2.1. parse a expression
-- Fail parser
failParser :: Parser a
failParser = Parser $ const Nothing

-- Parse a single character
charParser :: Char -> Parser Char
charParser c = Parser $ \s ->
    case s of
        (x:xs) | x == c -> Just (x, xs)
        _ -> Nothing

-- Helper function to satisfy a predicate on a character
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- anyChar
    if p c then return c else failParser

-- Parser for whitespace
whitespace :: Parser ()
whitespace = do
    _ <- many (satisfy isSpace)
    return ()

-- Parser that consumes any character
anyChar :: Parser Char
anyChar = Parser $ \s ->
    case s of
        (x:xs) -> Just (x, xs)
        _ -> Nothing

-- Parse an atom (variable, function, or parentheses)
parse_atom :: Parser Expr
parse_atom = parse_variable <|> parse_function <|> parse_macro <|> parse_parentheses

-- Parse an expression enclosed in parentheses
parse_parentheses :: Parser Expr
parse_parentheses = do
    charParser '('
    whitespace
    e <- parse_expr'
    whitespace
    charParser ')'
    return e

parse_expr_tail :: Parser [Expr]
parse_expr_tail = many (whitespace *> parse_atom)



-- Parse a variable
parse_variable :: Parser Expr
parse_variable = do
    c <- satisfy isLower
    return (Variable [c])
    

-- Parse a function
parse_function :: Parser Expr
parse_function = do
    charParser '\\'
    whitespace
    v <- satisfy isLower
    whitespace
    charParser '.'
    whitespace
    e <- parse_expr'
    --trace ("Parsed function: \\" ++ [v] ++ "." ++ show e) $ return (Function [v] e)
    return (Function [v] e)

-- Parse an application of two expressions
parse_application :: Parser Expr
parse_application = do
    e1 <- parse_atom
    es <- parse_expr_tail
    return $ foldl Application e1 es

parse_macro :: Parser Expr
parse_macro = do
    charParser '$'
    macroName <- many (satisfy isLower)
    -- trace ("Parsed macro: $" ++ macroName) $ return (Macro macroName)
    return (Macro macroName)

-- Parse an expression
parse_expr :: String -> Expr
parse_expr s = case parse parse_expr' s of
    Just (expr, "") -> expr
    _ -> Variable ""

parse_expr' :: Parser Expr
parse_expr' = parse_application <|> parse_atom <|> parse_macro


-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code = undefined
