x = 1
 
f :: Int -> Int -> Int
f x y = x * y
 
g = f 1
 
f1 :: (Int -> Int) -> Int
f1 x = (x 1) + 1
 
 
-- signatura unei functii nu e obligatorie
h :: a -> a
h x = x
 
{-
def f[A](x: A): A = x 
 
x => x
-}
 
{- compunerea de functii -}
comp :: (b -> c) -> (a -> b) -> a -> c 
comp f g x = f (g x)
--comp f g = \x -> f (g x)
 
{-
\x -> \y -> x + y   // functie anonima (lambda) in forma curry
\x y -> x + y       // forma uncurry (identica cu cea de sus)
 
-}
 
positive :: Int -> Int
positive = \x -> if x > 0 then x else -x
 
 
{- Pattern matching -}
factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)
{- In Haskell apelul de functie leaga cel mai strans
  <expr> f x 
-}
 
reduceRange :: Int -> Int -> Int 
--reduceRange start stop = if start > stop then 0 else start + reduceRange (start+1) stop
reduceRange start stop 
  | start > stop = 0
  | otherwise = start + reduceRange (start + 1) stop
 
 
reduceWith :: (Int -> Int -> Int) -> Int -> Int -> Int
reduceWith op start stop 
  | start > stop = 0
  | otherwise = start 0111p0032reduceWith op (start+1) stop
 
{-
  Putem transforma oricand, o functie PREFIX intr-una infix, si invers.
-}  
 
 
reduceOptim :: (Int -> Int -> Int) -> Int -> Int -> Int
reduceOptim op start stop = aux start
  where aux :: Int -> Int
        aux crt 
          | crt > stop = 0
          | otherwise = crt 0111p0032aux (crt + 1)
 
 
reduceLet :: (Int -> Int -> Int) -> (Int, Int) -> Int
reduceLet op (start,stop) = 
  let aux :: Int -> Int
      aux crt
          | crt > stop = 0
          | otherwise = crt 0111p0032aux (crt + 1)
  in aux start
 
{- Liste in Haskell -}
 
l1 = [1,2,3,4]
l2 = 1 : 2 : 3 : []
 
sumAll :: [Int] -> Int
sumAll [] = 0
sumAll (x:xs) = x + sumAll xs
 
size :: [a] -> Int
 
size [] = 0
size (_:xs) = 1 + size xs
 
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs
 
-- foldr :: (a -> b -> b) -> b -> [a] -> b
 
flatten2 :: [[a]] -> [a]
flatten2 = foldr (++) []
