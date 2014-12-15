module Main where

import Control.Concurrent
import Control.Concurrent.Thread
import Control.Concurrent.STM
import Data.List
import System.Random

import System.Environment


main :: IO ()
main =
  do
    tid <- forkIO $ randomDelay
    wait tid

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3, 15)
                 threadDelay (r * 10000)

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

sim :: (Integer -> IO ()) -> Integer -> IO ()
sim f num = do f num