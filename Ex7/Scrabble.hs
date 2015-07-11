{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

scoreInt :: Score -> Int
scoreInt (Score i) = i

instance Monoid Score where
  mempty = Score 0
  (Score a) `mappend` (Score b) = Score (a + b)

score :: Char -> Score
score 'A' = 1
score 'B' = 3
score 'C' = 3
score 'D' = 2
score 'E' = 1
score 'F' = 4
score 'G' = 2
score 'H' = 4
score 'I' = 1
score 'J' = 8
score 'K' = 5
score 'L' = 1
score 'M' = 3
score 'N' = 1
score 'O' = 1
score 'P' = 3
score 'Q' = 10
score 'R' = 1
score 'S' = 1
score 'T' = 1
score 'U' = 1
score 'V' = 4
score 'W' = 4
score 'X' = 8
score 'Y' = 4
score 'Z' = 10
score 'a' = 1
score 'b' = 3
score 'c' = 3
score 'd' = 2
score 'e' = 1
score 'f' = 4
score 'g' = 2
score 'h' = 4
score 'i' = 1
score 'j' = 8
score 'k' = 5
score 'l' = 1
score 'm' = 3
score 'n' = 1
score 'o' = 1
score 'p' = 3
score 'q' = 10
score 'r' = 1
score 's' = 1
score 't' = 1
score 'u' = 1
score 'v' = 4
score 'w' = 4
score 'x' = 8
score 'y' = 4
score 'z' = 10
score _ = 0


scoreString :: String -> Score
scoreString = sum . map score
