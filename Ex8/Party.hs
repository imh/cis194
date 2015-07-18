{- OPTIONS_GHC -Wall -fno=warn-orphans -}

module Party where

import Employee
import Data.Tree
import Data.List
import Data.Function

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = f}) (GL es ft) = GL (e:es) (f + ft)

glCons2 :: GuestList -> Employee -> GuestList
glCons2 (GL es ft) e@(Emp {empFun = f}) = GL (e:es) (f + ft)

instance Monoid (GuestList) where
  mempty = GL [] 0
  mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--treeFoldl :: (b -> a -> b) -> b -> Tree a -> b
--treeFoldl f x (Node a []) = f x a
--treeFoldl f x (Node a (b:bs)) = treeFoldl f (treeFoldl f x b) (Node a bs)

--treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
--treeFoldr f x (Node a []) = f a x
--treeFoldr f x (Node a (b:bs)) = treeFoldr f (treeFoldr f x b) (Node a bs)

--treeFoldl :: ([b] -> a -> b) -> Tree a -> b
--treeFoldl f (Node a forest) = f (map (treeFoldl f) forest) a

treeFold :: (a -> [b] -> b)  -> Tree a -> b
treeFold f (Node a forest) = f a (map (treeFold f) forest)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subMaxes =
  let (withBosses, withoutBosses) = unzip subMaxes
   in (glCons boss $ mconcat withoutBosses,
                     mconcat withBosses)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

companyFunString :: String -> String
companyFunString companyString =
  let company = read companyString
      gl = maxFun company
      fun = (\(GL _ f) -> f) gl
      funString = "Total fun: " ++ show fun
      emps = sort . map empName $ (\(GL es _) -> es) gl
   in unlines (funString:emps)

main :: IO ()
main = readFile "company.txt" >>= putStr . companyFunString
