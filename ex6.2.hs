-- ex6.2.hs

{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Capability = TravelToPast | TravelToFuture | TravelToAny
                deriving Show
                 
data TimeMachine = TimeMachine { _manufacturer :: String
                               , _model :: Int
                               , _name :: String
                               , _capability :: Capability
                               , _price :: Float }
                   deriving Show

makeLenses ''TimeMachine

updatePrice :: Float -> [TimeMachine] -> [TimeMachine]
updatePrice percent ts = ts & traversed.price %~ (\p -> p*(1+percent/100))