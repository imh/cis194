{-# OPTIONS_GHC -Wall #-}

module Ex2 where

import Log
import Text.Read

splitFirstSpace :: String -> (String, String)
splitFirstSpace xs = case (words xs) of
          (w:ws) -> (w, unwords ws)
          []     -> ("", "")
--splitFirstSpace xs = keepAddingTo [] xs
--  where keepAddingTo ys (' ':zs) = (ys, zs)
--        keepAddingTo ys (x:zs) = keepAddingTo (ys ++ [x]) zs
--        keepAddingTo ys [] = (ys, [])

tryParsePair :: (String, String) -> Maybe (Int, String)
tryParsePair (s1, s2) = case (readMaybe s1) of
  Just x  -> Just (x, s2)
  Nothing -> Nothing

parseNumAndString :: String -> Maybe (Int, String)
parseNumAndString cs = tryParsePair $ splitFirstSpace cs

parseMessage :: String -> LogMessage
parseMessage s@('I':' ':cs) = case (parseNumAndString cs) of
  Just (t, m) -> LogMessage Info t m
  Nothing     -> Unknown s
parseMessage s@('W':' ':cs) = case (parseNumAndString cs) of
  Just (t, m) -> LogMessage Warning t m
  Nothing     -> Unknown s
parseMessage s@('E':' ':cs) = case (parseNumAndString cs) of
  Just (n, tm) -> case (parseNumAndString tm) of
      Just (t, m) -> LogMessage (Error n) t m
      Nothing     -> Unknown s
  Nothing      -> Unknown s
parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert m@(LogMessage _ _ _) Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node lt nm@(LogMessage _ nt _) rt)
  | (t < nt) = Node (insert m lt) nm rt
  | otherwise        = Node lt nm (insert m rt)
insert _ (Node _ (Unknown _) _) = error "MessageTree must not contain Unkown messages, but one was found."

build :: [LogMessage] -> MessageTree
build lms = buildRec lms Leaf
 where buildRec (m:ms) mt = buildRec ms $ (insert m mt)
       buildRec [] mt = mt

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt nm rt) = (inOrder lt) ++ [nm] ++ (inOrder rt)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = map incompletePrinter $ inOrder $ build $ filter cond lm
  where cond (LogMessage (Error n) _ _) = n >= 50
        cond _ = False
        incompletePrinter (LogMessage _ _ s) = s
        incompletePrinter _ = error "We should have already filtered out the Unknowns."

foo :: [LogMessage] -> [String]
foo lm = [unlines $ whatWentWrong lm]


--instance Num b => Num (a -> b) where
--  f + g = (\x -> (f x) + (g x))
--  f * g = (\x -> (f x) * (g x))
--  abs = (abs .)
--  signum = (signum .)
--  fromInteger = const . fromInteger
--  negate = (negate .)

