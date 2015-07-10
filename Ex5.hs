{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Ex5 where

import Parser (parseExp)
import qualified StackVM as S
import qualified Data.Map as M
import Data.Maybe

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

instance Expr S.Program where
  mul x y = x ++ y ++ [S.Mul]
  add x y = x ++ y ++ [S.Add]
  lit x = [S.PushI x]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | VVar String
  deriving (Show)

instance Expr VarExprT where
  mul = VMul
  add = VAdd
  lit = VLit

instance HasVars VarExprT where
  var = VVar

instance Expr (M.Map String Integer -> Maybe Integer) where
  mul f g = \m -> let fm = f m
                      gm = g m
                   in if isNothing fm || isNothing gm
                       then Nothing
                       else Just $ (fromJust fm) * (fromJust gm)
  add f g = \m -> let fm = f m
                      gm = g m
                   in if isNothing fm || isNothing gm
                       then Nothing
                       else Just $ (fromJust fm) + (fromJust gm)
  lit i = \_ -> Just i

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = \m -> M.lookup s m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs