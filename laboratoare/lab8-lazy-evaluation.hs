-- 8.2.1
nats :: [Integer]
nats = 0 : map (+1) nats

-- 8.2.2
odds :: [Integer]
odds = 1 : map (+2) odds

squares :: [Integer]
squares = map (^2) nats

-- 8.2.3
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


data BTree = Node Int BTree BTree | Nil deriving Show
 
tree :: BTree
tree = Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil)

-- This is an infinite data type, no way to stop generating the tree
data StreamBTree = StreamNode Int StreamBTree StreamBTree

-- 8.3.1
repeatTree :: Int -> StreamBTree
repeatTree k = StreamNode k (repeatTree k) (repeatTree k)

-- 8.3.2
sliceTree :: Int -> StreamBTree -> BTree
sliceTree 0 _ = Nil
sliceTree k (StreamNode val left right) = Node val (sliceTree (k-1) left) (sliceTree (k-1) right)

-- 8.3.3
generateTree :: Int -> (Int -> Int) -> (Int -> Int) -> StreamBTree
generateTree k leftfunc rightfunc = StreamNode k (generateTree (leftfunc k) leftfunc rightfunc) (generateTree (rightfunc k) leftfunc rightfunc)

-- 8.4.1
build :: (Double -> Double) -> Double -> [Double]
build g a0 = a0 : build g (g a0)

-- 8.4.2
alternatingBinary :: [Double]
alternatingBinary = build (\x -> if x == 0 then 1 else 0) 0

alternatingCons :: [Double]
alternatingCons = build (\x -> if x >= 0 then -x - 1 else -x + 1) 0

alternatingPowers :: [Double]
alternatingPowers = build (\x -> -2 * x) 1

-- 8.4.3
select :: Double -> [Double] -> Double
select e (x1:x2:xs)
  | abs (x1 - x2) < e = x2
  | otherwise = select e (x2:xs)

-- 8.4.4
phiApprox :: Double
phiApprox = select 0.00001 ratios
  where
    ratios = zipWith (/) (map fromIntegral (tail fibs)) (map fromIntegral fibs)

-- 8.4.5
piApprox :: Double
piApprox = select 0.00001 (build (\an -> an + sin an) 1)

-- 8.4.6
sqrtApprox :: Double -> Double
sqrtApprox k = select 0.00001 (build (\an -> 1/2 * (an + k/an)) 1)

-- 8.4.7
testFunc :: Double -> Double
testFunc x = 2 * x * x + 3 * x + 1

generateApprox :: Double -> [Double]
generateApprox h0 = build (\x -> x/2) h0

generateDerivatives :: (Double -> Double) -> Double -> [Double]
generateDerivatives f a = map (\h -> (f (a+h) - f a) / h) (generateApprox 1)

derivativeApprox :: (Double -> Double) -> Double -> Double
derivativeApprox f a = select 0.00001 (generateDerivatives f a)
