-- ex4.4.hs

class Priceable p where
  price :: p -> Double
  
totalPrice :: Priceable p => [p] -> Double
totalPrice ps =  sum $ map price ps