{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Ex6 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib $ n - 1) + (fib $ n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a,b) -> (b, a+b)) (0, 1)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:(streamToList xs)

instance (Show a) => Show (Stream a) where
  show xs = show . (take 20) . streamToList $ xs

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons fx $ streamFromSeed f fx
  where fx = f x

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

xGen :: Stream Integer
xGen = Cons 0 (Cons 1 $ streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap (0-)
  (Cons a as) + (Cons b bs) = Cons (a+b) (as + bs)
  (Cons a as) * bSt@(Cons b bs) = Cons (a*b) ((fromInteger a)*bs + as*bSt)
  abs (Cons a as) = Cons (abs a) (abs as)
  signum (Cons a as) = Cons (signum a) (signum as)

--instance Fractional (Stream Integer) where
--  fromRational a = Cons a (streamRepeat 0)
--  (Cons a as) `div` (Cons b bs) = 