module Chapter13.Offer where

data Offer a = Present a | PercentDiscount Float | AbsoluteDiscount Float
  | Restrict [a] (Offer a)
  | From Integer (Offer a) | Until Integer (Offer a) | Extend Integer (Offer a)
  | Both (Offer a) (Offer a) | BetterOf (Offer a) (Offer a) | If (Expr a) (Offer a) (Offer a)
  deriving Show

noOffer :: Offer a
noOffer = AbsoluteDiscount 0

data Expr a = AmountOf a | PriceOf a
  | TotalNumberProducts | TotalPrice
  | IVal Integer | FVal Float
  | (Expr a) :+: (Expr a) | (Expr a) :*: (Expr a)
  | (Expr a) :<: (Expr a) | (Expr a) :<=: (Expr a)
  | (Expr a) :>: (Expr a) | (Expr a) :>=: (Expr a)
  | (Expr a) :&&: (Expr a) | (Expr a) :||: (Expr a) | Not (Expr a)
  deriving Show

v :: Offer String
v = Until 30 $ BetterOf (AbsoluteDiscount 10.0)
                        (Both (Present "balloon")
                              (If (TotalPrice :>: IVal 100) (PercentDiscount 5.0)
                                                            noOffer))

period :: Show a => Integer -> Integer -> Offer a -> Offer a
period f d o = From f (Until (f+d) o)

allOf :: Show a => [Offer a] -> Offer a
allOf = foldr Both noOffer

theOffer :: Offer String
theOffer = period 3 5 (allOf [Present "Balloon", Present "Muffin", PercentDiscount 10.0])
