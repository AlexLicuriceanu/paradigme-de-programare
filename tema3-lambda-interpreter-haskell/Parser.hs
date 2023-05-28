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

-- 2.1. parse a expression

-- Some general parsers.

-- Fail parser.
failParser :: Parser a
failParser = Parser $ \s -> Nothing

-- Parse a single character.
charParser :: Char -> Parser Char
charParser c = Parser $ \s ->
    case s of
        (x:xs) | x == c -> Just (x, xs)
        _ -> Nothing

-- Parser that consumes any character.
anyChar :: Parser Char
anyChar = Parser $ \s ->
    case s of
        (x:xs) -> Just (x, xs)
        _ -> Nothing

-- Parse a character based on a predicate.
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s ->
    case s of
        (x:xs) | p x -> Just (x, xs)
        _ -> Nothing

-- Parse whitespaces.
whitespaceParser :: Parser ()
whitespaceParser =
    do
        _ <- many (predicateParser isSpace)
        return ()



-- Parse an atom. (variable, function, macro or parentheses)
parse_atom :: Parser Expr
parse_atom = parse_variable <|> parse_function <|> parse_macro <|> parse_parentheses

-- Parse a variable.
parse_variable :: Parser Expr
parse_variable =
    do
        c <- predicateParser isLower
        return (Variable [c])


-- Parse a function.
parse_function :: Parser Expr
parse_function =
    do
        charParser '\\'
        v <- predicateParser isLower
        charParser '.'
        e <- parse_atom <|> parse_expr'

        return (Function [v] e)


-- Parse the rest of the expression.
parse_expr_tail :: Parser [Expr]
parse_expr_tail =
    do
        es <- many (whitespaceParser *> parse_atom)
        return es


-- Parse an application of two expressions
parse_application :: Parser Expr
parse_application =
    do
        e <- parse_atom
        es <- parse_expr_tail
        return (foldl Application e es)


-- Parse an application of two functions.
parse_application' :: Parser Expr
parse_application' =
    do
        f1 <- parse_function
        whitespaceParser
        f2 <- parse_function
        return (Application f1 f2)

-- Parse an expression in parentheses.
parse_parentheses :: Parser Expr
parse_parentheses =
    do
        charParser '('
        e <- parse_expr'
        charParser ')'
        return e


-- Parse a macro.
parse_macro :: Parser Expr
parse_macro =
    do
        charParser '$'
        macroName <- many (predicateParser isLower)
        --trace ("parsing macro: $" ++ macroName) $ return (Macro macroName)
        return (Macro macroName)


-- Top level function to parse a string expression.
parse_expr :: String -> Expr
parse_expr s = case parse parse_expr' s of
    Just (expr, "") -> expr
    _ -> Variable ""

-- Helper function for parse_expr.
parse_expr' :: Parser Expr
parse_expr' = parse_application' <|> parse_application <|> parse_atom



-- 4.2. parse code
-- Top level function to parse code.
parse_code :: String -> Code
parse_code s = case parse parse_code' s of
    Just (code, "") -> code
    _ -> Evaluate (Variable "")

-- Helper function for parse_code.
parse_code' :: Parser Code
parse_code' = parse_assign <|> parse_evaluate

-- Parse a macro assign command.
parse_assign :: Parser Code
parse_assign =
    do
        key <- many (predicateParser isLower)
        whitespaceParser
        charParser '='
        whitespaceParser
        e <- parse_expr'
        return (Assign key e)

-- Parse an evaluate command.
parse_evaluate :: Parser Code
parse_evaluate =
    do
        e <- parse_expr'
        return (Evaluate e)