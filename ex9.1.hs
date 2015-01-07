{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import System.Random

promptMessage :: Int -> IO ()
promptMessage tries =
  if tries < 5
    then putStrLn "Not correct, try again!"
    else putStrLn "You have used up opportunities, quitting..."

guess :: Int -> Int -> IO Bool
guess tries actual =
  if tries <= 5
    then do (g :: Int) <- fmap read getLine
            if g == actual
              then return True
              else promptMessage tries >> guess (tries + 1) actual
    else return False

main :: IO ()
main =
  do
    putStrLn "Please guess a number between 1 and 10:"
    rn <- randomRIO (1, 10)
    b <- guess 1 rn
    when b $ putStrLn $ "You are correct! The number is: " ++ show rn