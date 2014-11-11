-- ex5.1.hs

primes :: [Integer]
primes = helper [2..]
  where helper l = let n = head l in
                   n : helper (filter (\x -> x `mod` n /= 0) l)