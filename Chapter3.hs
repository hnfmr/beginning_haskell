-- Chapter3.hs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f lst = helper lst []
    where helper [] aux     = aux
          helper (x:xs) aux
            | f x       = helper xs (x:aux)
            | otherwise = helper xs aux
            
data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show
                 
infMax :: (Num a, Ord a) => InfNumber a -> InfNumber a -> InfNumber a
infMax MinusInfinity x       = x
infMax x MinusInfinity       = x
infMax PlusInfinity _        = PlusInfinity
infMax _ PlusInfinity        = PlusInfinity
infMax (Number x) (Number y) = Number (max x y)