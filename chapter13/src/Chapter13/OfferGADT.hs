{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter13.OfferGADT where

data Offer a =
    Present a
  | PercentDiscount Float
  | AbsoluteDiscount Float
  | Restrict [a] (Offer a)
  | From Integer (Offer a)
  | Until Integer (Offer a)
  | Extend Integer (Offer a)
  | Both (Offer a) (Offer a)
  | BetterOf (Offer a) (Offer a)
  | If (Expr a Bool) (Offer a) (Offer a)

noOffer :: Offer a
noOffer = AbsoluteDiscount 0

data Expr a r where
  AmountOf              :: a -> Expr a Integer
  PriceOf               :: a -> Expr a Float
  TotalNumberProducts   :: Expr a Integer
  TotalPrice            :: Expr a Float
  Val                   :: Num n => n -> Expr a n
  (:+:)                 :: Num n => Expr a n -> Expr a n -> Expr a n
  (:*:)                 :: Num n => Expr a n -> Expr a n -> Expr a n
  (:<:)                 :: Num n => Expr a n -> Expr a n -> Expr a n
  (:<=:)                :: Num n => Expr a n -> Expr a n -> Expr a n
  (:>:)                 :: Num n => Expr a n -> Expr a n -> Expr a n
  (:>=:)                :: Num n => Expr a n -> Expr a n -> Expr a n
  (:&&:)                :: Expr a Bool -> Expr a Bool -> Expr a Bool
  (:||:)                :: Expr a Bool -> Expr a Bool -> Expr a Bool
  Not                   :: Expr a Bool -> Expr a Bool
  -- deriving Show

priceOf :: Eq a => a -> [(a, Float)] -> Expr a Float
priceOf x xs = let p = find x xs in
  PriceOf p
  where find _ [] = error "not in list"
        find p (l:ls) = if p == fst l then p else find p ls

interpretExpr :: Eq a => Expr a t -> [(a, Float)] -> t
interpretExpr (e1 :+: e2)  list = interpretExpr e1 list + interpretExpr e2 list
interpretExpr (e1 :||: e2) list = interpretExpr e1 list || interpretExpr e2 list
interpretExpr (PriceOf a) list = snd (find a list)
  where find _ [] = error "not in list"
        find p (l:ls) = if p == fst l then l else find p ls


