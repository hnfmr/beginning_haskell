-- ex3.1.hs

swapTriple :: (a,b,c) -> (b,c,a)
swapTriple (x,y,z) = (y,z,x)

duplicate :: a -> (a, a)
duplicate x = (x, x)

nothing :: a -> Maybe a
nothing _ = Nothing

index :: [a] -> [ (Int, a) ]
index [] = []
index [x] = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
               in (n+1,x):indexed

maybeA :: [a] -> Char
maybeA [] = 'a'
