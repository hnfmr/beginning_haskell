-- ex7.6.hs

-- Reference: https://gist.github.com/edofic/9429263

{-# LANGUAGE PackageImports #-}
import "mtl" Control.Monad.State
 
factState :: StateT Integer (State Integer) ()
factState = do
  n <- get
  when (n > 1) $ do
    modify $ subtract 1
    lift $ modify (*n)
    factState
 
factorial x = execState (execStateT factState x) 1