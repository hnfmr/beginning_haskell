-- Client.hs
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Map as M
import qualified Data.Set as S

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Ord, Eq)

data Person = Person { firstName :: String, lastName :: String }
              deriving (Show, Ord, Eq)

data ClientKind = GovOrgKind | CompanyKind | IndividualKind
                  deriving (Show, Eq, Ord)

kind' :: Client Integer -> ClientKind
kind' GovOrg{..}     = GovOrgKind
kind' Company{..}    = CompanyKind
kind' Individual{..} = IndividualKind

classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients  = foldr f M.empty
    where f c m = let k = kind' c in
                  case M.lookup k m of
                    Just s -> M.insert k (S.insert c s) m
                    Nothing -> M.insert k (S.insert c S.empty) m


classifyClients' :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients' cs =
  let s1 = S.fromList $ filter (\case GovOrg{..} -> True
                                      _          -> False) cs in
  let s2 = S.fromList $ filter (\case Company{..} -> True
                                      _           -> False) cs in
  let s3 = S.fromList $ filter (\case Individual{..} -> True
                                      _              -> False) cs in
  let m1 = M.insert GovOrgKind s1 M.empty in
  let m2 = M.insert CompanyKind s2 m1 in
  let m3 = M.insert IndividualKind s3 m2 in
  m3
  

