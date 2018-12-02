{-# OPTIONS_GHC -Wall             #-}
{-# Language TypeSynonymInstances #-}


module Five where

import ExprT
import Parser
import StackVM (Program)

----------
-- 1

eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

----------
-- 2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

----------
-- 3

class Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e 

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

----------
-- 4

instance Expr Integer where
    lit i = i
    add = (+)
    mul = (*)

instance Expr Bool where
    lit i | i <= 0 = False | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
instance Expr MinMax where
    lit i = MinMax i
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Ord, Show)
instance Expr Mod7 where
    lit = Mod7 . mod 7
    add (Mod7 i1) (Mod7 i2) = Mod7 $ mod 7 $ i1 + i2
    mul (Mod7 i1) (Mod7 i2) = Mod7 $ mod 7 $ i1 * i2

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

----------
-- 5


compile :: String -> Maybe Program
compile = undefined
