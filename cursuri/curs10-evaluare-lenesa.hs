{-
   In Haskell evaluarea este lazy
 
 -}
ones :: [Int]
ones = 1:ones
 
-- axioma pt take: take n (x:xs) = x:(take (n-1) xs)
{-
  take 2 ones
  take 2 (1:ones)
  1:take 1 ones
  1:take 1 (1:ones)
  1:1:take 0 ones
  1:1:[]
 
-}
call = foldr (&&) True [True,False,False,False,True]
{-
 
 
True && (foldr (&&) True [False,False,False,True] )
foldr (&&) True [False,False,False,True]
 
False && (foldr (&&) True [False,False,True])
False 
 
 
foldr op acc x:xs = x 0111p0032foldr op acc xs
 
foldl op acc x1:xs = foldl op (acc 0111p0032x1) xs
foldl op acc x2:xs = foldl op ((acc 0111p0032x1) 0111p0032x2) xs
 
foldl'
 
 
strict = aplicativ
non-strict = lazy
 
 
 
In evaluarea lazy, direct-recursiv /= ineficient
 
Ce putem programa cu evaluare lenesa?
  - programarea cu liste "aparent infinite"
  - programare dinamica
 
-}
 
-- lista "infinita" a numerelor naturale
nats :: [Int]
nats = nextNat 0
  where nextNat i = i : nextNat (i+1)
