{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Json where

import DataBuilder

import Data.Aeson
import Data.Text
import Data.Aeson.Types
import Control.Applicative

import qualified Data.HashMap.Strict as M

clientToJSON :: Client Integer -> Value
clientToJSON (GovOrg i n) =
  object [ "type"     .= String "govorg"
         , "id"       .= Number (fromInteger i)
         , "name"     .= String (pack n) ]
clientToJSON (Company i n p d) =
  object [ "type"     .= String "company"
         , "id"       .= Number (fromInteger i)
         , "name"     .= String (pack n)
         , "person"   .= personToJSON p
         , "duty"     .= String (pack d) ]
clientToJSON (Individual i p) =
  object [ "type"     .= String "individual"
         , "id"       .= Number (fromInteger i)
         , "person"   .= personToJSON p ]

personToJSON :: Person -> Value
personToJSON (Person f l) = object [ "first"  .= String (pack f)
                                   , "last"   .= String (pack l) ]
-- data Product = Product { id :: Int, name :: String, price :: Double, descr :: String } deriving (Show, Ord, Eq)

-- data Purchase = Purchase { client :: Client Int, products :: [Product] } deriving (Show, Ord, Eq)
instance ToJSON Product 
instance FromJSON Product

instance ToJSON Purchase
instance FromJSON Purchase

-- jsonToPerson :: Value -> Maybe Person
-- jsonToPerson (Object o) = do String f <- M.lookup "first" o
--                              String l <- M.lookup "last" o
--                              return $ Person (unpack f) (unpack l)
-- jsonToPerson _          = Nothing

jsonToPerson :: Value -> Parser Person
jsonToPerson (Object o) = Person <$> o .: "first" <*> o .: "last"
jsonToPerson _          = Control.Applicative.empty

instance ToJSON Person where
  toJSON = personToJSON
  
instance FromJSON Person where
  parseJSON = jsonToPerson

jsonToClient :: FromJSON i => Value -> Parser (Client i)
jsonToClient (Object o) =
  case M.lookup "type" o of
    Just (String "govorg")     -> GovOrg <$> o .: "id" <*> o .: "name"
    Just (String "company")    -> Company <$> o .: "id" <*> o .: "name"
                                          <*> o .: "person" <*> o .: "duty"
    Just (String "individual") -> Individual <$> o .: "id" <*> o .: "person"
    _                          -> Control.Applicative.empty
jsonToClient _ = Control.Applicative.empty

instance ToJSON (Client Integer) where
  toJSON = clientToJSON

instance FromJSON i => FromJSON (Client i) where
  parseJSON = jsonToClient

