{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DataBuilder where

import GHC.Generics
import Data.Text
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B


import Data.Monoid((<>), mconcat)


data Person = Person { firstName :: String, lastName :: String }
              deriving (Show, Ord, Eq, Generic)

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Ord, Eq, Generic)

data Product = Product { id :: Integer, name :: String, price :: Double, descr :: String } deriving (Show, Ord, Eq, Generic)

data Purchase = Purchase { client :: Client Integer, products :: [Product] } deriving (Show, Ord, Eq, Generic)

escapeString :: String -> Text
escapeString = replace "\n" "\\n" . replace "," "\\," . replace "(" "\\(" .
               replace ")" "\\)" . pack

personToText :: Person -> B.Builder
personToText (Person f l) =
  "person(" <> B.fromText (escapeString f) <> B.singleton ','
            <> B.fromText (escapeString l) <> B.singleton ')'
               
clientToText :: Client Integer -> B.Builder
clientToText (GovOrg i n) =
  "client(gov," <> B.decimal i <> B.singleton ',' <> B.fromText (escapeString n) <> B.singleton ')'

clientToText (Company i n p d) =
  "client(com," <> B.decimal i <> B.singleton ',' <> B.fromText (escapeString n) <> B.singleton ','
                <> personToText p <> B.singleton ',' <> B.fromText (escapeString d) <> B.singleton ')'

clientToText (Individual i p) =
  "client(ind," <> B.decimal i <> B.singleton ',' <> personToText p <> B.singleton ')'

productToText :: Product -> B.Builder
productToText (Product i n p d) =
  "product(" <> B.decimal i <> B.singleton ',' <> B.fromText (escapeString n) <> B.singleton ','
                  <> B.realFloat p <> B.singleton ',' <> B.fromText (escapeString d) <> B.singleton ')'

productsToText :: [Product] -> B.Builder
productsToText = mconcat . go
  where go [] = []
        go [p] = [productToText p]
        go (p:ps) = productToText p : B.singleton ',' : go ps
             
purchaseToText :: Purchase -> B.Builder
purchaseToText (Purchase c ps) =
  "purchase(" <> clientToText c <> B.singleton ',' <> B.singleton '[' <> productsToText ps <> B.singleton ']' <> ")"
