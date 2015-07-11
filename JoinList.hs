{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module JoinList where
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
m1 +++ m2 = Append (tag m1 <> tag m2) m1 m2

intSize :: Sized a => a -> Int
intSize = getSize . size

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = intSize . tag

indexJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ a) | i == 0 = Just a
                      | otherwise = Nothing
indexJ i (Append b a1 a2) | i > intSize b = Nothing
                          | i < jlSize a1 = indexJ i a1
                          | otherwise = indexJ (i - (jlSize  a1)) a2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_  !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)


jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ a1 a2) = jlToList a1 ++ jlToList a2

dropJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl | i <= 0 = jl
dropJ _ a@(Single _ _) = a
dropJ i (Append sj a b) | i >= intSize sj = Empty
                        | i < jlSize a = dropJ i a +++ b
                        | otherwise = dropJ (i - (jlSize a)) b

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i <= 0 = Empty
takeJ i a@(Single _ _) | i == 1 = a
                       | otherwise = Empty
takeJ i ab@(Append sj a b) | i >= intSize sj = ab
                           | i <= jlSize a = takeJ i a
                           | otherwise = a +++ (takeJ (i - (jlSize a)) b)

--jl = Single 1 "abc" +++ Single 1 "d" +++ Single 1 "efgh" :: JoinList Size String
--takeTest i = (jlToList $ takeJ i jl) == (take i $ jlToList jl)

scoreLine :: String -> JoinList Score String
scoreLine x = (Single (scoreString x) x)

--instance (Monoid a, Monoid b) => Monoid (a,b) where
--  mempty = (mempty, mempty)
--  mappend (a1, b1) (a2, b2) = (mappend a1 a2, mappend b1 b2)

instance Buffer (JoinList (Score, Size) String) where
  toString = concat . jlToList
  fromString s = Single (scoreString s, 1) s
  line = indexJ
  replaceLine i s jl = takeJ (i-1) jl +++
                              fromString s +++
                                      dropJ i jl
  numLines = intSize . snd . tag
  value = scoreInt . fst . tag

main = runEditor editor $ foldl (+++) mempty (map fromString
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ])
