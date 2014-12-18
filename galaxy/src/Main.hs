{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}

module Main where

import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad
import System.Environment
import System.Random

data GalaxyMessage = LookForGalaxy ProcessId | GalaxyFound String
                   deriving (Typeable, Generic)

instance Binary GalaxyMessage

traveller :: Process ()
traveller = do LookForGalaxy master <- expect
               -- Look for galaxies in space-time
               send master (GalaxyFound "Andromeda")

remotable ['traveller]               

master :: [NodeId] -> Process ()
master nodes =
  do myPid <- getSelfPid
     mapM_ (\node -> do pid <- spawn node $(mkStaticClosure 'traveller)
                        send pid (LookForGalaxy myPid))
           nodes
     forever $ do GalaxyFound g <- expect
                  say $ "Found galaxy: " ++ g

main :: IO ()
main = do args <- getArgs
          case args of
            ["master", host, port] -> do
              backend <- initializeBackend host port (Main.__remoteTable initRemoteTable)
              startMaster backend master
            ["traveller", host, port] -> do
              backend <- initializeBackend host port (Main.__remoteTable initRemoteTable)
              startSlave backend
            _ -> do putStrLn "Unknown parameters"
            