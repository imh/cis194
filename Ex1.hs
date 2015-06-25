{- OPTIONS_GHC -Wall -}

module Ex1 where

toDigits :: Integral a
         => a   --Number
         -> [a] --The digits of the input number if positive, else []
toDigits x
  | x <= 0    = []
  | otherwise = loop x
      where loop 0 = []
            loop y = (loop $ (y - rem) `div` 10) ++ [rem]
              where rem = y `mod` 10

rev :: [a] -- A list
    -> [a] -- That list backwards
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

toDigitsRev :: Integral a
            => a   -- A number
            -> [a] -- That number backwards ([] if negative)
toDigitsRev = rev . toDigits

doubleEveryOtherL :: Integral a
                 => [a] -- A list of numbers
                 -> [a] -- That list with even elements doubled
doubleEveryOtherL [] = []
doubleEveryOtherL (x:[]) = [x]
doubleEveryOtherL (x:y:zs) = x:(2*y):(doubleEveryOtherL zs)

doubleEveryOther :: Integral a
                 => [a] -- A list of numbers
                 -> [a] -- That list with even elements doubled counting from the right
doubleEveryOther = rev . doubleEveryOtherL . rev

sumDigits :: Integral a
          => [a] -- A list of numbers
          -> a   -- The sum of the digits of those numbers
sumDigits [] = 0
sumDigits (x:xs) = (s $ toDigits x) + (sumDigits xs)
  where s [] = 0
        s (x:xs) = x + (s xs)

validate :: Integral a
         => a    -- A number
         -> Bool -- Whether that number is a valid credit card number (ignoring length)
validate x
  | (sumDigits $ doubleEveryOther $ toDigits x) `mod` 10 == 0 = True
  | otherwise = False
