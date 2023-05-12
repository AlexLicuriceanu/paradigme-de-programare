import Data.Array
 
t0 :: [Int]
t0 = [2,3,5,1,4]
t1 :: [Int]
t1 = [2,3,5,1,4,6,7,1,2,3,5,3,4,7,1,2,3,4,5,6,7,5,3,4,5,6,7,6,3,2]
t3 :: [Int]
t3 = [2,3,5,1,4,6,7,1,2,3,5,3,4,7,1,2,3,4,5,6,7,5,3,4,5,6,7,6,3,2,2,3,5,1,4,6,7,1,2,3,5,3,4,7,1,2,3,4,5,6,7,5,3,4,5,6,7,6,3,2,2,3,5,1,4,6,7,1,2,3,5,3,4,7,1,2,3,4,5,6,7,5,3,4,5,6,7,6,3,2,2,3,5,1,4,6,7,1,2,3,5,3,4,7,1,2,3,4,5,6,7,5,3,4,5,6,7,6,3,2]
t2 :: [Int]
t2 = [2,3,5,1,4,6,7,1,2,3,5,3,4,7,1,2,3,4,5,6]
 
-- the operation (!!)
 
-- recursive solution:
sol0 :: [Int] -> Int
sol0 prices = max_profit 0 ((length prices) - 1) 1
	where max_profit left right year 
			| left > right = 0
			| otherwise = maximum [year * (prices !! left) + (max_profit (left+1) right (year+1)),
								   year * (prices !! right) + (max_profit left (right-1) (year+1)) ]
 
 
-- list comprehensions (in Haskell) echivalentul for expressions din Scala
-- list-array
 
sol1 :: [Int] -> Int
sol1 l = matrix ! (0,n-1)
	where 
		n = length l
		prices = listArray (0,n-1) l
		bounds = ((0,0), (n-1,n-1))
		matrix = listArray bounds [max_profit l r | (l,r) <- range bounds]
		year l r = n - (r - l)
		max_profit left right
			| left == right = (year left right) * (prices ! left)
			| otherwise = maximum [(year left right) * (prices ! left) + (matrix ! (left+1, right)),
			                       (year left right) * (prices ! right) + (matrix ! (left, right-1))]
