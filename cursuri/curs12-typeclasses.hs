-- import Prelude hiding (show, Show)
-- import qualified Prelude as Prelude 
 
{-
Part 1: Algebraic/Abstract datatypes
-}
 
data Nat = Zero | Succ Nat -- deriving Show
 
add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ y) x = Succ (add y x)
 
toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ x) = 1 + toInt x
 
{-
    3 + 1 
    3 + 2 + 1
 
-}
data Expr = Atom Int | Mult Expr Expr | Plus Expr Expr 
 
type Age = Int
 
data Student = 
  Student Int (String, String) Int |
  Absolvent Int (String, String)
 
eval :: Expr -> Int
eval (Atom x) = x
eval (Mult e1 e2) = (eval e1) * (eval e2)
eval (Plus e1 e2) = (eval e1) + (eval e2)
 
e1 = Mult (Atom 1) (Atom 2)
 
varsta :: Student -> Int
varsta (Student x _ _) = x
varsta (Absolvent x _) = x
 
{-  tipuri de date polimorfice!!   -}
 
data List a = Void | Cons a (List a)
 
{-
In Haskell programam cu functii obisnuite in lumea valorilor,
inclusiv constructori de date
 
programam cu constructori de tip in lumea tipurilor, atunci 
construim tipuri de date noi, DAR NU NUMAI!
-}
 
{- Sa presupunem ca vrem sa afisam expresii -}
 
showExpr :: Expr -> String
showExpr (Atom x) = Prelude.show x
showExpr (Plus e1 e2) = (showExpr e1) ++ " + " ++(showExpr e2)
showExpr (Mult e1 e2) = (showExpr e1) ++ " * " ++(showExpr e2)
 
showNat :: Nat -> String
showNat = Prelude.show . toInt
-- compunerea de functii
 
{- Pt fiecare tip de date nou, trebuie sa TINEM MINTE numele fiecareia
 
  Am dori: UN SINGUR NUME de functie "show", cu mai multe implementari.
  Polimorfism ad-hoc (vs polimorfism parametric)
 
  Vom folosi o constructie dedicata in Haskell:
 -}
 
{-
class Show a where
  show :: a -> String
-}
 
{-
  Show reprezinta O FAMILIE (o multime, sau colectie) de tipuri, 
  sunt acele tipuri care pot fi afisate.
 
  Tipurile a din clasa Show sunt acele tipuri care suporta si implementeaza
  metoda show :: a -> String
 
  Cum inrolam tipuri noi intr-o clasa:
-}
 
 
instance Show Nat where
  show = Prelude.show . toInt
 
instance Show Expr where
  show (Atom x) = Prelude.show x
  show (Plus e1 e2) = (show e1) ++ " + " ++ (show e2)
  show (Mult e1 e2) = (show e1) ++ " * " ++ (show e2)
 
 
instance Eq Nat where
  Zero == Zero = True
  (Succ x) == (Succ y) = x == y
  _ == _ = False
 
 
-- daca "a" este un membru al clasei Eq
-- atunci "List a" este un membru al clasei Eq
 
instance (Eq a) => Eq (List a) where
  Void == Void = True
  (Cons x xs) == (Cons y ys) = x == y && xs == ys
  _ == _ = False
 
{- Ce altceva putem inrola intr-un type-class? -}
 
-- Exista deja in Haskell:
-- data Maybe a = Nothing | Just a  
 
data Option a = None | Some a deriving Show
 
-- o functie care se comporta exact ca map:
applyL :: (a -> b) -> (List a) -> (List b)
applyL f Void = Void
applyL f (Cons x xs) = Cons (f x) (applyL f xs)
 
 
applyO :: (a -> b) -> (Option a) -> (Option b)
applyO f None = None
applyO f (Some x) = Some (f x)
 
 
{-
    ?1 - ar trebui sa fie o colectie cu elemente tip a
                   sa un constructor de tip ce primeste tipul a
 
    ?2 - ar trebui sa fie ACEEASI COLECTIE, dar cu elemente de tip b
-}
-- t este un constructor de tip avand kind-ul * => *
 
{-
class Functor t where
  fmap :: (a -> b) -> t a -> t b
-}
class Apply t where
  apply :: (a -> b) -> t a -> t b 
 
instance Functor List where
  fmap f Void = Void
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
 
instance Functor Option where 
  fmap f None = None
  fmap f (Some x) = Some (f x)
 
-- f :: (a -> b) -> a -> b
f g y = g y
 
 
{- Operatia infix fmap este <$>. Este foarte utila pentru a scrie cod
   eficient si compact, la fel cum este si $. -}
