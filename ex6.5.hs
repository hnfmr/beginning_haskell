-- ex

{-
import Control.Monad

newtype Maybe' a = Maybe' (Maybe a)

instance Monad Maybe' where
  return x = Maybe' (Just x)
  m >>= f = case m of
              Maybe' Nothing -> Maybe' Nothing
              Maybe' (Just x) -> f x
-}

numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId _ = return 2

productIdByPurchaseId :: Integer -> Maybe Integer
productIdByPurchaseId _ = return 39283

priceByProductId :: Integer -> Maybe Double
priceByProductId _ = return 29394.2

thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = Nothing
thenDo (Just x) f = f x

purchaseValue' :: Integer -> Maybe Double
purchaseValue' purchaseId =
  numberItemsByPurchaseId purchaseId `thenDo` (\n ->
    productIdByPurchaseId purchaseId `thenDo` (\productId ->
      priceByProductId productId     `thenDo` (\price ->
        Just $ (fromInteger n) * price)))

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  numberItemsByPurchaseId purchaseId >>= (\n ->
    productIdByPurchaseId purchaseId >>= (\productId ->
      priceByProductId productId     >>= (\price ->
        Just $ (fromInteger n) * price)))