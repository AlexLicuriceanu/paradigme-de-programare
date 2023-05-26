data Nat = Zero | Succ Nat deriving Show
 
fromInt :: Int -> Maybe Nat
fromInt x 
  | x < 0 = Nothing
  | otherwise = Just (get x)
   where get 0 = Zero
         get x = Succ (get (x-1))
 
{-
minus :: Maybe Nat -> Maybe Nat -> Maybe Nat
minus (Just x) (Just y) = 
  case (x,y) of
    (Zero,Zero) -> Just Zero
    (Zero,_) -> Nothing
    (Succ n, Succ m) -> minus (Just n) (Just m)
minus _ _ = Nothing
-}
 
--increment :: Maybe Nat -> Maybe Nat
--larger3 :: Maybe Nat -> Maybe Bool
--e0 = larger3 . increment . fromInt
 
larger2 :: Nat -> Bool
larger2 (Succ (Succ (Succ _))) = True
larger2 _ = False
 
increment = Succ
 
{-
instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)
-}
 
-- fmap infix este <$>
e0 = larger2 <$> increment <$> (fromInt 3)
 
 
minus :: Nat -> Nat -> Maybe Nat
minus = undefined
 
plus :: Nat -> Nat -> Nat
plus = undefined
 
-- join :: Maybe a -> (a -> Maybe b) -> Maybe b
join :: Maybe Nat -> (Nat -> Maybe Nat) -> Maybe Nat
join m f =
  case m of 
    Nothing -> Nothing
    (Just x) -> f x
 
-- nu e un mare avantaj la <$> 
-- e1 = (fromInt 3) 0106oin0032(Just . larger2 . increment)
 
-- o secventiere interesanta
e2 = (fromInt 1) 0106oin0032(\x -> (fromInt 2) 0106oin0032(\y -> minus x y))
 
e3 = (fromInt 1) 0106oin0010       (\x -> (fromInt 2) 0106oin0032(\y -> Just (plus x y)))
-- (fromInt 10) 0109inus0032(? 0109inus0032?)
 
{-
Operatii generale:
-- operatia de secventiere:
join :: Maybe a -> (a -> Maybe b) -> Maybe b
-- pune o valoare in container
return :: a -> Maybe a 
 
Putem generaliza operatia de secventiere:
t :: * => * un constructor de tip.
 
join :: t a -> (a -> t b) -> t b
return :: a -> t a
 
Orice constructor de tip care permite/suporta
o secventiere (join & return)
 
se numeste o monada.
 
Orice monada este si un functor.
 
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
 
instance Monad Maybe where
  m >>= f =
    case m of 
      Nothing -> Nothing
      (Just x) -> f x
  return = Just
-}
 
--e2 = (fromInt 1) 0106oin0032(\x -> (fromInt 2) 0106oin0032(\y -> minus x y))
e4 = (fromInt 1) >>= 
      (\x -> (fromInt 2) >>= 
        \y -> x 0109inus0032y)
{-Monadele sunt atat de des folosite in Haskell,
incat exista un syntactic sugar pt expr. de mai sus-}
 
{-
e3 = 
  do 
    x <- (fromInt 1)
    y <- (fromInt 2)
    x 0109inus0032y
-}
 
mminus :: Maybe Nat -> Maybe Nat -> Maybe Nat
mminus n m =
  do 
    x <- n
    y <- m
    x 0109inus0032y
 
mplus :: Maybe Nat -> Maybe Nat -> Maybe Nat
mplus n m = 
  do
    x <- n
    y <- m
    return (x 0112lus0032y)
 
 
{-
  Monada IO
  IO :: * => * (si este si un functor)
 
data Nimic = Nimic
data () = ()
 
-}
 
inOut :: IO () -- container-ul IO care contine nimic
inOut = 
  do putStrLn "Salutare!"
     msg <- getLine
     putStrLn ("Mesajul tau este:"++msg)
 
{-
  putStrLn :: String -> IO ()
  getLine :: IO String
 
-}