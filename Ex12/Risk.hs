{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Random
import Data.List
import GHC.Exts

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Eq, Show)

deathsFromRolls :: [DieValue] -> [DieValue] -> (Int, Int)
deathsFromRolls atts defs =
  let battles = zipWith compare (sortWith Down atts) (sortWith Down defs)
      foo (attDeaths, defDeaths) GT = (attDeaths, defDeaths + 1)
      foo (attDeaths, defDeaths) _ = (attDeaths + 1, defDeaths)
   in foldl foo (0,0) battles

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield atts defs) = do
  (attRands :: [DieValue]) <- getRandoms
  (defRands :: [DieValue]) <- getRandoms
  let atts' = min 3 $ max 0 $ atts - 1
      defs' = min 2 defs
      attDice = take atts' $ attRands
      defDice = take defs' $ defRands
      (attDeaths, defDeaths) = deathsFromRolls attDice defDice
  return (Battlefield (max 0 $ atts - attDeaths) (max 0 $ defs - defDeaths))

successProb :: Battlefield -> Rand StdGen Double
successProb battlefield = do
  let n = 10000000
  bfs <- sequence . map battle . replicate n $ battlefield
  let bfs' = filter (\bf -> defenders bf == 0) bfs
  return ((realToFrac $ length bfs') / (realToFrac n))

main :: IO ()
main = (evalRandIO . successProb $ Battlefield 5 2) >>= print




















