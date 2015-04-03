{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, EmptyDataDecls, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs, OverloadedStrings #-}

module Sqlite where

import Gender

import qualified Database.Persist as P
import Database.Esqueleto
import Database.Persist.TH
-- import Database.Persist.Sqlite(runSqlite)
import Data.Int(Int64)

-- mkPersist sqlSettings [persistLowerCase|
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country
  name      String
  canWeSend Bool default=True
  UniqueCountry name
  deriving Show
Product
  p_id    Int64
  name    String
  price   Double
  descr   String
  deriving Show
Client
  firstName String
  lastName  String
  address   String
  country   CountryId
  age       Int Maybe
  gender    Gender Maybe
  UniqueClient firstName lastName address country
  deriving Show
Purchase
  client   ClientId
  product  ProductId
  number   Int
  amount   Double
  deriving Show
|]

-- exampleConn :: IO ()
-- exampleConn = runSqlite "example.db" $ do
--   runMigration migrateAll
--   spain    <- insert $ Country "Spain" True
--   prod     <- insert $ Product 1 "Soap" 1234.2 "Hazelnut"
--   _client1 <- insert $ Client "Alex" "Serre" "Home Town 1" spain (Just 25) (Just Male)
--   _purchase1 <-  insert $ Purchase _client1 prod 10 102993.2
--   return ()

-- getPeopleOver25 :: SqlPersistT IO [Client]
getPeopleOver25 :: SqlPersistT IO [Entity Client]
getPeopleOver25 =
  select $
  from $ \client -> do
  where_ (client ^. ClientAge >. just (val 25))
  orderBy [ asc (client ^. ClientLastName), asc (client ^. ClientFirstName) ]
  return client
