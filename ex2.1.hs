-- file: beginning/ex2.1.hs

-- 2
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- 3
onlyOne :: [a] -> Bool
onlyOne (a:[]) = True
onlyOne _      = False

-- 4
concList :: [[a]] -> [a]
concList [] = []
concList (x:xs) = x ++ (concList xs)