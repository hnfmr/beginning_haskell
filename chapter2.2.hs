-- chapter2 record
{-# LANGUAGE RecordWildCards #-}
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
                       
greet :: ClientR -> String
greet IndividualR { person = PersonR { .. } } = "Hi, " ++ firstName
greet CompanyR { .. }                         = "Hello, " ++ clientRName
greet GovOrgR { }                             = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
    let newName = (toUpper initial):rest
    in p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p