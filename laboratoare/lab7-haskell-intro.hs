-- 7.2.1
fact :: Int -> Int
fact x = if x == 0 then 1 else x * fact(x-1)

-- 7.2.2
mygcd :: Int -> Int -> Int
mygcd a b = if b == 0 then a else mygcd b (a `mod` b)

-- 7.2.3
mysqrt :: Double -> Double
mysqrt x = x ** 0.5

-- 7.2.4
mymin :: [Int] -> Int
mymin = foldr (\x acc -> if x < acc then x else acc) 99999

mymax :: [Int] -> Int
mymax = foldr (\x acc -> if x > acc then x else acc) (-99999)

-- 7.2.5
unique :: [Int] -> [Int]
unique = foldr (\x acc -> if elem x acc then acc else x:acc) []

-- 7.2.6 & 7.2.7
fizzbuzz :: [Int] -> [String]
fizzbuzz [] = []
fizzbuzz (x:xs)
    | x `mod` 105 == 0 = "FizzBuzzBazz" : fizzbuzz xs
    | x `mod` 35 == 0  = "BuzzBazz" : fizzbuzz xs
    | x `mod` 21 == 0  = "FizzBazz" : fizzbuzz xs
    | x `mod` 15 == 0  = "FizzBuzz" : fizzbuzz xs
    | x `mod` 7 == 0   = "Bazz" : fizzbuzz xs
    | x `mod` 3 == 0   = "Fizz" : fizzbuzz xs
    | x `mod` 5 == 0   = "Buzz" : fizzbuzz xs
    | otherwise        = show x : fizzbuzz xs

-- 7.3.1
f :: Int -> Int
f x = x * 2

mymapl :: (a -> b) -> [a] -> [b]
mymapl f = foldl (\acc x -> acc ++ [f x]) []

mymapr :: (a -> b) -> [a] -> [b]
mymapr f = foldr (\x acc -> [f x] ++ acc) []

-- 7.3.2
g :: Int -> Bool
g x = if x `mod` 2 == 0 then True else False

myfilterl :: (a -> Bool) -> [a] -> [a]
myfilterl f = foldl (\acc x -> if f(x) then acc ++ [x] else acc) []

myfilterr :: (a -> Bool) -> [a] -> [a]
myfilterr f = foldr (\x acc -> if f(x) then [x] ++ acc else acc) []

-- 7.3.3
sumWith :: Int -> Int -> Int
sumWith acc x = acc + x

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f z xs = foldr (\x acc -> f acc x) z (reverse xs)

-- 7.3.4
bubbleSort :: [Int] -> [Int]
bubbleSort xs = bubbleSort' xs (length xs)

bubbleSort' :: [Int] -> Int -> [Int]
bubbleSort' xs 0 = xs
bubbleSort' xs n = bubbleSort' (bubble xs) (n-1)

bubble :: [Int] -> [Int]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs)
    | x > y = y : bubble (x:xs)
    | otherwise = x : bubble (y:xs)

-- 7.3.5
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
  where smaller = filter (<x) xs
        larger = filter (>=x) xs
