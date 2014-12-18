module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.List
import System.Random

fromYear :: Integer
fromYear = 1900

toYear :: Integer
toYear = 1920

supportedYears :: [Integer]
supportedYears = [fromYear .. toYear]

randomYear :: IO Integer
randomYear =
  do
    g <- newStdGen
    return $ fst $ randomR (fromYear, toYear) g

forkDelay :: Int -> IO ()-> IO ()
forkDelay n f = replicateM_ n $ forkIO (randomDelay >> f)

main :: IO ()
main =
  do
    ys <- replicateM 10 randomYear
    print $ nub ys
    stYs <- newTVarIO $ nub ys
    forkDelay 10 $ randomYear >>= \y -> atomically $ sim stYs y `orElse` return () 

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3, 15)
                 threadDelay (r * 100000)

canTravel1 :: TVar Integer -> STM Bool
canTravel1 clientNum =
  do
    num <- readTVar clientNum
    if num > 8
    then return False
    else return True

canTravel2 :: Integer -> TVar [Integer] -> STM Bool
canTravel2 y years =
  do
    ys <- readTVar years
    return $ y `notElem` ys

sim :: TVar [Integer] -> Integer -> STM ()
sim ys y =
  do
    --cond1 <- canTravel1 y
    cond2 <- canTravel2 y ys
    if not cond2
    --if not cond1  || not cond2
      then retry
      else return ()
