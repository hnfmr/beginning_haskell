{-# LANGUAGE OverloadedStrings #-}
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

