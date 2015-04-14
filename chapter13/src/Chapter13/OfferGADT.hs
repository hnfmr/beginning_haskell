{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}

module Chapter13.OfferGADT where

data Offer a = Present a | PercentDiscount Float | AbsoluteDiscount Float
  | Restrict [a] (Offer a)
  | From Integer (Offer a) | Until Integer (Offer a) | Extend Integer (Offer a)
  | Both (Offer a) (Offer a) | BetterOf (Offer a) (Offer a) | If (Expr a Bool) (Offer a) (Offer a)

noOffer :: Offer a
noOffer = AbsoluteDiscount 0

data Expr a r where
  AmountOf              :: a -> Expr a Integer
  PriceOf               :: a -> Expr a Float
  TotalNumberProducts   :: Expr a Integer
  TotalPrice            :: Expr a Float
  IVal                  :: Integer -> Expr a Integer
  FVal                  :: Float -> Expr a Float
  (:+:)                 :: Num n => Expr a n -> Expr a n -> Expr a n
  (:*:)                 :: Num n => Expr a n -> Expr a n -> Expr a n
  (:<:)                 :: Num n => Expr a n -> Expr a n -> Expr a n
  (:<=:)                :: Num n => Expr a n -> Expr a n -> Expr a n
  (:>:)                 :: Num n => Expr a n -> Expr a n -> Expr a n
  (:>=:)                :: Num n => Expr a n -> Expr a n -> Expr a n
  (:&&:)                :: Expr a Bool -> Expr a Bool -> Expr a Bool
  (:||:)                :: Expr a Bool -> Expr a Bool -> Expr a Bool
  Not                   :: Expr a Bool -> Expr a Bool

interpretExpr :: Eq a => Expr a t -> [(a, Float)] -> t
interpretExpr (e1 :+: e2)  list = interpretExpr e1 list + interpretExpr e2 list
interpretExpr (e1 :||: e2) list = interpretExpr e1 list || interpretExpr e2 list
interpretExpr e list =
  let newl = map (\(p, f) -> (pof p, f)) list in
  foldr (\(_, f) acc -> f + acc) 0.0 newl


