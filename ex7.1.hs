-- ex7.1.hs

import Control.Monad

brokenThreeJumps :: Int -> [Int]
brokenThreeJumps year =
  let jumpList = [(-1),3,5] in
    do 
      x <- jumpList
      y <- jumpList
      z <- jumpList
      return $ year+x+y+z

brokenJumps :: Int -> Int -> [Int] -> [Int]
brokenJumps year times jumpList =
  let offsets = map sum (replicateM times jumpList) in
  map (year+) offsets