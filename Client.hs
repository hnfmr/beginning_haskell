-- Client.hs
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map as M
import qualified Data.Set as S

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Ord, Eq)

data Person = Person { firstName :: String, lastName :: String }
              deriving (Show, Ord, Eq)

data ClientKind = GovOrgKind | CompanyKind | IndividualKind deriving (Show, Eq, Ord)

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

