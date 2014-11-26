-- ex7.5.hs

import Control.Monad

sequence' :: Monad m => [m a] -> m [a]
sequence' ms = foldr (\m acc -> do
                                  x <- m
                                  l <- acc
                                  return $ x:l) (return []) ms