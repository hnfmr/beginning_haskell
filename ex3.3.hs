-- ex3.3.hs
{-# LANGUAGE RecordWildCards #-}

import Chapter2

productP :: Num a => [a] -> a
productP [] = 1
productP (x:xs) = x * (productP xs)

product' :: Num a => [a] -> a
product' = foldr (*) 1

getClientName :: ClientR -> String
getClientName GovOrgR { .. } = clientRName
getClientName CompanyR { .. } = clientRName
getClientName IndividualR { person = PersonR { .. } } = firstName ++ " " ++ lastName

minClient :: ClientR -> ClientR -> ClientR
minClient c1 c2 = let c1Name = getClientName c1 in
                  let c2Name = getClientName c2 in
                  if (length c1Name) <= (length c2Name)
                  then c1
                  else c2
                  

-- must not be empty, how can that be enforced?
minimumClient :: [ClientR] -> ClientR
minimumClient cs =
  foldr f (head cs) cs
  where f c a = minClient c a

allP :: [Bool] -> Bool
allP []     = False
allP (x:xs) = x && (allP xs) 

all' :: [Bool] -> Bool
all' = foldr (\e a -> e && a) True

allL :: [Bool] -> Bool
allL = foldl (\a e -> a && e) True
