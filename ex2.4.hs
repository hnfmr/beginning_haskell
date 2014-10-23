-- ex2.4.hs
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

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
                                 
updatePrice :: TimeMachineR -> Float -> TimeMachineR
updatePrice (machine@TimeMachineR {..}) discount =
  let newPrice = price * discount
  in machine { price = newPrice }

clientName :: Client -> String
clientName (GovOrg name)         = name
clientName (Company name _ _ _)  = name
clientName (Individual person _) = case person of
                                     Person fn ln _ -> fn ++ " " ++ ln

countGender :: [Client] -> (Int, Int)
countGender clients = count clients (0, 0)
    where count [] stats     = stats
          count (c:cs) stats =
            case c of
              Company _ _ (Person _ _ g) _ -> count cs (updateCounter g stats)
              Individual (Person _ _ g) _  -> count cs (updateCounter g stats)
              _                            -> count cs stats
          updateCounter g (ms, fs) = case g of
                                     Male    -> (ms+1, fs)
                                     Female  -> (ms, fs+1)
                                     Unknown -> (ms, fs)
                                     
applyDiscount :: [TimeMachine] -> Float -> [TimeMachine]
applyDiscount machines discount = map (f discount) machines
    where f discount (TimeMachine manufacturer model name capa price) =
              TimeMachine manufacturer model name capa (price * discount)
              
ackermann :: Int -> Int -> Int
ackermann m n
  | m == 0          = n + 1
  | m > 0 && n == 0 = ackermann (m-1) 1
  | m > 0 && n > 0  = ackermann (m-1) (ackermann m (n-1))
  
unzip' :: [(a,b)] -> ([a], [b])
unzip' ts = helper ts ([], [])
    where helper [] aux                = aux
          helper ((x,y):es) (as', bs') = helper es (as'++[x], bs'++[y])
          
responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Qi")       = True
specialClient (responsibility -> "Director") = True

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
                       
greet :: ClientR -> String
greet IndividualR { person = PersonR { .. } } = "Hi, " ++ firstName
greet CompanyR { .. }                         = "Hello, " ++ clientRName
greet GovOrgR { }                             = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
    let newName = (toUpper initial):rest
    in p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p