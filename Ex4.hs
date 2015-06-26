{-# OPTIONS_GHC -Wall #-}

module Ex4 where

import Data.List ((\\))

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3*n+1)

data Tree a = Leaf
				    | Node Integer (Tree a) a (Tree a)
	deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node i _ _ _) = i

insert :: a -> Tree a -> Tree a
insert y Leaf = Node 0 Leaf y Leaf
insert y (Node i l x r)
	| il < ir 	= Node i (insert y l) x r
	| il > ir   = Node i l x r2
	| otherwise = Node (ir2+1) l x r2
  where il = height l
        ir = height r
        r2 = insert y r
        ir2 = height r2

validHeight :: Tree a -> Bool
validHeight Leaf = True
validHeight (Node 0 Leaf _ Leaf) = True
validHeight (Node 0 _ _ _) = False
validHeight (Node i l _ r) = depthmatch && validHeight l && validHeight r
	where depthmatch = i == 1 + (max (height l) (height r))

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldl (/=) False

-- ((:) . f) is just "apply f to the first argument, then
--  stick it on the front of the next one"
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = foldr . flip

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 1:ss
  where nums = [1..n]
        ijs = [(i,j) | i <- [1..n], j <- [1..i]]
        fs = map (\(i,j) -> i + j + 2*i*j) ijs
        filtered = nums \\ fs --[num | num <- nums, not $ num `elem` fs]
        ss = map (\x -> 2*x + 1) filtered
