-- ex6.6.hs

--import Control.Monad
import Data.Monoid

newtype MyWriter m a = MyWriter (a,m)

instance Monoid m => Monad (MyWriter m) where
	return x = MyWriter (x, mempty)
	MyWriter (a, ma) >>= g = let MyWriter(b, mb) = g a in
	                         MyWriter (b, mb <> ma)

tell :: Monoid m => m => MyWriter m a -> MyWriter m ()
tell m (MyWriter (_, mx)) = MyWriter ((), m <> mx)