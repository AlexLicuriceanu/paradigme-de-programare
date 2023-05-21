import Data.Char
import Control.Applicative

data Nat = Zero | Succ Nat deriving Show
data Expr = Atom Int | Var String | Plus Expr Expr  deriving Show
-- helper to do the parsing for us
data Parser a = Parser (String -> [(a,String)])
parse (Parser p) s = p s


 
fromInt :: Int -> Maybe Nat 
fromInt x 
  | x < 0 = Nothing
  | otherwise = Just $ get x
      where get 0 = Zero
            get x = Succ (get (x-1))

-- 10.1.1
mminus :: Maybe Nat -> Maybe Nat -> Maybe Nat
mminus (Just m) (Just n) = minus m n
mminus _ _ = Nothing

minus :: Nat -> Nat -> Maybe Nat
minus m Zero = Just m
minus Zero _ = Nothing
minus (Succ m) (Succ n) = minus m n

mplus :: Maybe Nat -> Maybe Nat -> Maybe Nat
mplus (Just m) (Just n) = plus m n
mplus _ _ = Nothing

plus :: Nat -> Nat -> Maybe Nat
plus m Zero = Just m
plus Zero n = Just n
plus (Succ m) (Succ n) = plus m (Succ(Succ n))

-- 10.1.2
mmulti :: Maybe Nat -> Maybe Nat -> Maybe Nat
mmulti (Just m) (Just n) = multi m n
mmulti _ _ = Nothing

multi :: Nat -> Nat -> Maybe Nat
multi m Zero = Just Zero
multi Zero n = Just Zero
multi (Succ m) (Succ n) =
    do
        product <- multi m n
        add <- mplus (Just m) (Just product)
        mplus (Just add) (Just (Succ n))

-- 10.2.0
failParser :: Parser a 
failParser = Parser $ \s -> []

-- 10.2.1
--If we need to parse 'A', we use this function to return us a parser that parses 'A'.
charParser :: Char -> Parser Char
charParser c = Parser $ \s -> case s of
    (x:xs) -> [(c, xs)]
    _ -> []

-- 10.2.2
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> case s of
    (x:xs) | p x -> [(x, xs)]
    _ -> []

instance Monad Parser where
  mp >>= f = Parser $ 
    \s -> case parse mp s of 
        [] -> []
        [(val,rest)] -> parse (f val) rest
 
  return x = Parser $ \s -> [(x,s)]
 
instance Applicative Parser where
  af <*> mp = 
    do 
      f <- af
      v <- mp
      return $ f v
  pure = return
 
instance Functor Parser where 
  fmap f mp = 
    do 
      x <- mp
      return $ f x
 
instance Alternative Parser where
  empty = failParser
  p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                [] -> parse p2 s
                                x -> x


-- 10.2.3
varP :: Parser String
varP =
    do
        firstChar <- predicateParser isLetter
        remainingChars <- many (predicateParser isAlphaNum)
        return (firstChar : remainingChars)

-- 10.2.4
{-
starParser will apply p zero or more times:
     - fails only after several applications
plusParser will apply p one or more times:
     - fails if it does not produce progress or after several applications
In short, plusParser needs to parse 'at least once', while starParser can go the plusParser route or just stop and fail after not parsing anything.
-}
plusParser :: (Parser a) -> Parser [a]
plusParser p =
    do
        first <- p
        rest <- starParser p
        return (first : rest)

starParser :: (Parser a) -> Parser [a]
starParser p = plusParser p <|> pure []

-- 10.2.5
varParser :: Parser String
varParser = do
    first <- predicateParser isLetter
    rest <- starParser (predicateParser isAlphaNum)
    return (first : rest)
 
varExprParser :: Parser Expr
varExprParser = do
    name <- varParser
    return (Var name)

-- 10.2.6
whitespaceChars :: String
whitespaceChars = " \t\n"

oneOf :: String -> Parser Char
oneOf s = predicateParser (`elem` s)

whitespaceParser :: Parser String
whitespaceParser = plusParser (foldr (\c acc -> acc <|> charParser c) empty whitespaceChars)
