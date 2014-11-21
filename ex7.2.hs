-- ex7.2.hs

import Control.Monad

find_ :: (a -> Bool) -> [a] -> Maybe a
find_ p l =                    
  let pl = map (\e -> if p e then Just e else Nothing) l in
  msum pl