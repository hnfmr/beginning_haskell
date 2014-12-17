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
    ys <- newTVar $ replicateM 10 randomYear
    forkDelay 10 $ atomically $ sim ys 1919

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3, 15)
                 threadDelay (r * 100000)

canTravel1 :: Integer -> STM Bool
canTravel1 clientNum =
  do
    if clientNum > 8
    then return False
    else return True

canTravel2 :: TVar [Integer] -> STM Bool
canTravel2 years =
  do
    ys <- readTVar years
    return $ (nub ys) /= ys

sim :: TVar [Integer] -> Integer -> STM ()
sim ys y =
  do
    cond1 <- canTravel1 y
    cond2 <- canTravel2 ys
    if not cond1  || not cond2
      then retry
      else return ()
