-- ex9.1.hs

--guessOk :: (Int, Int) -> Int -> Bool
import Control.Monad

guess :: Int -> IO Bool
guess tries actual =
  do
    when (tries <= 5) $ do (g :: Int) <- fmap read getLine
                           if g == actual
                             then return True
                             else guess (tries + 1) actual

