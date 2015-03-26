{-# LANGUAGE DeriveGeneric #-}

module Person where

import Data.Binary
import GHC.Generics

data Person = Person { firstName :: String, lastName :: String }
              deriving (Show, Read, Generic)

instance Binary Person


