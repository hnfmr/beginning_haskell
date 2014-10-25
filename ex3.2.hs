-- ex3.2.hs

import Chapter2

filterOnes :: (Num a, Eq a) => [a] -> [a]
filterOnes xs = filter (== 1) xs

filterANumber :: (Num a, Eq a) => [a] -> a -> [a]
filterANumber xs x = filter (== x) xs

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f xs = filter (not . f) xs

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs cs = filter (\c -> case c of
                                   GovOrg _ -> True
                                   _        -> False
                          ) cs
                          
