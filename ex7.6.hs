-- ex7.6.hs

import Control.Monad.State

factorialT :: StateT Integer (State Integer) Integer
--factorialT = undefined
factorialT = do
                s <- get
                lift . put $ s * (execState (execStateT factorialT s*(s-1)) (s-1))
                return $ s-1
               