module Lambda where

import Expr
import Data.List
import Debug.Trace

-- 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
-- If the expression is a variable, return the variable name.
free_vars (Variable v) = [v]
-- If the expression is a function, call free_vars recursively and delete
-- the argument's name from the list of free variables.
free_vars (Function v e) = filter (/= v) (free_vars e)
-- If the expression is an application, call free_vars for the left and right
-- expressions, concatenate the results then remove duplicates with nub.
free_vars (Application e1 e2) = nub (free_vars e1 ++ free_vars e2)


-- Helper function that concatenates "x" and a given number.
generateName :: Int -> String
generateName n = "x" ++ show n

-- Function to generate a new name for a variable.
generateX :: [String] -> String
-- Generate [x1, x2, x3, ...], remove the variables that exist in both this new list and
-- the usedVars list and return the first element.
generateX usedVars = (filter (`notElem` usedVars) (map generateName [1..])) !! 0


-- Function to rename a variable in an expression. Parameters: old name, new name, expression.
rename :: String -> String -> Expr -> Expr
-- If the expression is a variable, check if the variable to rename is the variable
-- in the expression and return a new variable with the new name.
rename x y (Variable z) = 
    if z == x
        then Variable y
        else Variable z
-- If the expression is a function. If the variable to replace is the same as the lambda's variable,
-- return the initial function. Else, replace variable z recursively in the expression's body with a
-- newly generated variable name.
rename x y (Function z e) =
    if z == x
        then Function z e
        else Function newX (rename x y newExpr) where
            usedVars = free_vars e
            newX = generateX usedVars
            newExpr = rename z newX e
-- Recursively apply the rename function on both expressions.
rename x y (Application e1 e2) = Application (rename x y e1) (rename x y e2)



-- 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
-- If the expression is the same as the reduction variable, replace the expression with
-- the given variable, else return the original expression. 
reduce (Variable x) y e =
    if x == y
        then e
        else Variable x

reduce (Function x e1) y e2
    -- Check if the variable x matches the reduction variable y.
    | x == y = Function x e1
    -- Check if the variable x is free in expression e2. If not, apply
    -- the reduction to the body of the e1 expression.
    | x `notElem` free_vars e2 = Function x (reduce e1 y e2)
    -- If x is free in expression e2, generate a new variable name, rename
    -- all occurrences of x in expression e1 then apply the reduction to the
    -- renamed expression.
    | otherwise = Function newX (reduce newExpr y e2) where
        usedVars = free_vars e1 ++ free_vars e2
        newX = generateX usedVars
        newExpr = rename x newX e1
    
            
-- Recursively apply the reduction on both expressions.
reduce (Application e1 e2) y e3 = Application (reduce e1 y e3) (reduce e2 y e3)


-- Function to check if an expression is a value.
isValue :: Expr -> Bool
isValue (Variable v) = True
isValue (Function v e) = True
isValue _ = False

-- Normal Evaluation
-- 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Application (Function x e1) e2) = reduce e1 x e2
stepN (Application e1 e2) =
    if isValue e1
        then Application e1 (stepN e2)
        else Application (stepN e1) e2

stepN (Function x e) = Function x (stepN e)
stepN e = e

-- 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN e =
    if stepN e == e
        then e
        else reduceN (stepN e)


reduceAllN :: Expr -> [Expr]
reduceAllN e =
    if isValue e
        then [e]
        else e : reduceAllN (stepN e)

-- Applicative Evaluation
-- 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Application (Function x e1) e2) =
  if isValue e2
    then reduce e1 x e2
    else Application (Function x e1) (stepA e2)

stepA (Application e1 e2) =
  if isValue e1
    then Application e1 (stepA e2)
    else Application (stepA e1) e2

stepA (Function x e) = Function x (stepA e)
stepA e = e


-- 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA e = 
    if stepA e == e
        then e
        else reduceA (stepA e)

reduceAllA :: Expr -> [Expr]
reduceAllA e =
    if isValue e
        then [e]
        else e : reduceAllA (stepA e)

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros = undefined

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined
