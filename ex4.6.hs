-- ex4.5.hs ex4.6.hs

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving Show

data Person = Person { firstName :: String, lastName :: String }
              deriving Show

data ClientKind = GovOrgKind | CompanyKind | IndividualKind
                  deriving (Show, Eq, Ord)

instance Eq Person where
  Person{firstName=fnm1,lastName=lnm1}
    == Person{firstName=fnm2,lastName=lnm2} = fnm1==fnm2 && lnm1==lnm2

instance Eq i => Eq (Client i) where
  GovOrg{clientId=cid1,clientName=cnm1}
    == GovOrg{clientId=cid2,clientName=cnm2} = cid1==cid2 && cnm1==cnm2
  Company{clientId=cid1,clientName=cnm1,person=p1,duty=d1}
    == Company{clientId=cid2,clientName=cnm2,person=p2,duty=d2} =
      cid1==cid2 && cnm1==cnm2 && p1==p2 && d1==d2
  Individual{clientId=cid1,person=p1}
    == Individual{clientId=cid2,person=p2} = cid1==cid2 && p1==p2
  _ == _ = False

instance Ord Person where
  Person{firstName=fnm1,lastName=lnm1}
    <= Person{firstName=fnm2,lastName=lnm2} = fnm1<=fnm2 && lnm1<=lnm2
    
instance (Eq i, Ord i) => Ord (Client i) where
  Individual{clientId=cid1,person=p1}
    <= Individual{clientId=cid2,person=p2} = cid1<=cid2 && p1<=p2
  Individual{} <= GovOrg{} = False
  Individual{} <= Company{} = False
  
  Company{} <= GovOrg{} = False
  Company{} <= Individual{} = True
  Company{clientId=cid1,clientName=cnm1,person=p1,duty=d1}
    <= Company{clientId=cid2,clientName=cnm2,person=p2,duty=d2}
      = cid1<=cid2 && cnm1<=cnm2 && p1<=p2 && d1<=d2
      
  GovOrg{} <= Individual{} = True
  GovOrg{} <= Company{} = True
  GovOrg{clientId=cid1,clientName=cnm1}
    <= GovOrg{clientId=cid2,clientName=cnm2} = cid1<=cid2 && cnm1<=cnm2
