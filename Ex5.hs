{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Ex5 where

import Parser (parseExp)
import qualified StackVM as S

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  mul :: a -> a -> a
  add :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  mul = Mul
  add = Add
  lit = Lit

instance Expr Integer where
  mul = (*)
  add = (+)
  lit = id

instance Expr Bool where
  mul = (&&)
  add = (||)
  lit x = x <= 0

newtype MinMax = MinMax Integer deriving (Show, Eq)

instance Expr MinMax where
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)
  lit = MinMax

newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr Mod7 where
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  lit x = Mod7 (x `mod` 7)


reify :: ExprT -> ExprT
reify = id