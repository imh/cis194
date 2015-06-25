{- OPTIONS_GHC -Wall -}

module Ex3 where

import Data.List

takeEvery :: Int -> [a] -> [a]
takeEvery n = reverse . fst . foldl acc ([], 0)
  where acc (ys, i) x = if (i+1) `mod` n == 0
                        then (x:ys, 0)
                        else (ys, i+1)

skips :: [a] -> [[a]]
skips xs = map (flip takeEvery $ xs) [1..(length xs)]


localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:ds) = tryMax (a,b,c) ++ localMaxima ds
  where tryMax (a,b,c) = if b > a && b > c
                          then [b]
                          else []
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs =
  let
      valCount ys = (head ys, (length ys) - 1)
      gathered = ((map valCount) . group . sort) (xs ++ [0..9])
      maxCount = foldl' max 0 $ map snd gathered
      cols = map (\(i, n) -> replicate (maxCount - n) ' ' ++
                             replicate n '*' ++
                             "=" ++
                             show i) gathered
  in concat $ map (++"\n") $ transpose cols