module Lambda where

import Expr
import Data.List
import Debug.Trace

-- 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars (Variable v) = [v]
free_vars (Function v e) = filter (/= v) (free_vars e)
free_vars (Application e1 e2) = nub (free_vars e1 ++ free_vars e2)


-- Helper function that concatenates "x" and a given number.
generateName' :: Int -> String
generateName' n = "x" ++ show n

-- Function to generate a new name for a variable.
generateName :: [String] -> String
generateName usedVars = (filter (`notElem` usedVars) (map generateName' [1..])) !! 0


-- Function to rename a variable in an expression. Parameters: old name, new name, expression.
rename :: String -> String -> Expr -> Expr
rename x y (Variable z) = 
    if z == x
        then Variable y
        else Variable z

rename x y (Function z e) =
    if z == x
        then Function z e
        else Function newX (rename x y newExpr) where
            usedVars = free_vars e
            newX = generateName usedVars
            newExpr = rename z newX e

rename x y (Application e1 e2) = Application (rename x y e1) (rename x y e2)



-- 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce (Variable x) y e =
    if x == y
        then e
        else Variable x

reduce (Function x e1) y e2
    | x == y = Function x e1
    | x `notElem` free_vars e2 = Function x (reduce e1 y e2)
    | otherwise = Function newX (reduce newExpr y e2) where
        usedVars = free_vars e1 ++ free_vars e2
        newX = generateName usedVars
        newExpr = rename x newX e1

reduce (Application e1 e2) y e3 = Application (reduce e1 y e3) (reduce e2 y e3)


-- Function to check if an expression is a variable or a function.
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

-- 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros dict (Variable x) =
    case lookup x dict of
        Just e -> e
        _ -> Variable x

evalMacros dict (Function x e) = Function x (evalMacros dict e)
evalMacros dict (Application e1 e2) = Application (evalMacros dict e1) (evalMacros dict e2)
evalMacros dict (Macro m) =
    case lookup m dict of
        Just e -> evalMacros dict e
        _ -> Macro m


-- 4.1. evaluate code sequence using given strategy

-- Update a macro in the dictionary.
updateMacro :: [(String, Expr)] -> String -> Expr -> [(String, Expr)]
updateMacro dict key e = (key, e) : filter (\(k, _) -> k /= key) dict

-- Top level function for evaluating code.
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strat lines = evalCode' strat lines []

-- Helper function for evalCode.
evalCode' :: (Expr -> Expr) -> [Code] -> [(String, Expr)] -> [Expr]
evalCode' _ [] _ = []

evalCode' strat ((Evaluate e) : lines) dict =
    (strat (evalMacros dict e)) : evalCode' strat lines dict

evalCode' strat ((Assign key e) : lines) dict =
    evalCode' strat lines (updateMacro dict key e)
