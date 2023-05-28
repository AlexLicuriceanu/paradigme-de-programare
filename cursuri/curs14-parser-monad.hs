import Data.Char
import Control.Applicative
 
data Expr = Atom Int | 
            Var String | 
            Plus Expr Expr deriving Show
 
 
--type Parser a = String -> [(a,String)]
-- Parser :: * => *
data Parser a = Parser (String -> [(a,String)])
 
parse :: Parser a -> String -> [(a,String)]
parse (Parser p) s = p s 
 
--parseAtom :: Parser Expr
--parse parseAtom "1 + 2" = [(1, "+ 2")]
 
{- parsere mai generale: -}
failParser :: Parser a
failParser = Parser $ \s -> []
 
charParser :: Char -> Parser Char
charParser c = Parser $ \s -> case s of 
                                [] -> []
                                (x:xs) -> if x == c then [(c,xs)] else []
 
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $
  \s -> case s of 
          [] -> []
          (x:xs) -> if p x then [(x,xs)] else []
 
 
instance Monad Parser where
  --(>>=) :: m a -> (a -> m b) -> m b
  mp >>= f =
    Parser $ \s -> case parse mp s of
                      [] -> []
                      [(x,s')] -> parse (f x) s' 
  --return :: a -> m a
  return x = Parser $ \s -> [(x,s)]
 
 
instance Applicative Parser where
  mf <*> ma =
    do f <- mf
       x <- ma
       return $ f x 
  pure = return
 
instance Functor Parser where
--  fmap :: (a -> b) -> m a -> m b
  fmap f m = m >>= \x -> return $ f x
   {-
    do
      x <- m 
      return f x
   -}
 
whatParser :: Parser String
whatParser =
  do x <- predicateParser isAlpha
     y <- predicateParser isAlphaNum
     return [x,y]
 
plusParser :: (Parser a) -> Parser [a]
plusParser p =
  do x <- p
     xs <- starParser p
     return (x:xs)
 
instance Alternative Parser where
  empty = failParser
  p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                [] -> parse p2 s
                                ok -> ok
 
starParser :: (Parser a) -> Parser [a]
starParser p = (plusParser p) <|> (return []) -- <|> alternativa
 
varParser :: Parser String
varParser = 
  do 
    x <- predicateParser isAlpha
    xs <- starParser $ (predicateParser isAlphaNum)
    return (x:xs)
{-
data Expr = Atom Int | 
            Var String | 
            Plus Expr Expr deriving Show
-}
varExprParser :: Parser Expr
varExprParser = Var <$> varParser
 
 
valExprParser :: Parser Expr
valExprParser = (Atom . read) <$> plusParser (predicateParser isNumber)
 
atomicParser :: Parser Expr
atomicParser = valExprParser <|> varExprParser
 
{-
  <expr> ::= 
      <val_atomica> |
      <val_atomica> + <expr>
 
  <val_atomica> ::= <atom> sau <variabila>
 
-}
whiteSpaceParser :: Parser String
whiteSpaceParser = starParser (charParser ' ')
 
plusExprParser :: Parser Expr
plusExprParser =
  do whiteSpaceParser
     v <- atomicParser
     whiteSpaceParser
     charParser '+'
     whiteSpaceParser
     e <- exprParser
     whiteSpaceParser
     return $ Plus v e
 
exprParser :: Parser Expr
exprParser = plusExprParser <|> atomicParser
