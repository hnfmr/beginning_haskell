{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter2 where

import Data.Char

data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show
            
data Person = Person String String Gender
            deriving Show
            
data Gender = Male | Female | Unknown
            deriving Show
            
type Manufacturer = String
type Model = Int
type Name = String
data Capability = TravelToPast | TravelToFuture | TravelToAny
                deriving Show
type Price = Float
data TimeMachine = TimeMachine Manufacturer Model Name Capability Price
                 deriving Show
                 
data TimeMachineR = TimeMachineR { manufacturer :: Manufacturer
                                 , model :: Model
                                 , name :: Name
                                 , capability :: Capability
                                 , price :: Price }
                    deriving Show
                    
-- chapter2 record
data ClientR = GovOrgR  { clientRName :: String }
             | CompanyR { clientRName :: String
                        , companyId   :: Integer
                        , person      :: PersonR
                        , duty        :: String }
             | IndividualR { person :: PersonR }
             deriving Show
             
data PersonR = PersonR { firstName :: String
                       , lastName  :: String
                       } deriving Show