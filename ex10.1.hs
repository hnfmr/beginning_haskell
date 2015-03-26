{-# LANGUAGE OverloadedStrings #-}
import DataBuilder

import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text.Lazy as LT

import qualified Data.Text.Lazy.Builder as B

import Data.Conduit
import qualified Data.Conduit.Text as T
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Binary as CB

purchases :: [Purchase]
purchases = [ Purchase (GovOrg 1 "SFAF") [ Product 2 "Mouse" 19293.2 "Logic", Product 3 "Keybd" 29334.2 "HHKP" ],
              Purchase (Company 2 "FJF" (Person "Fl" "Ln") "Procure") [ Product 3 "Googse" 293.2 "Frolic", Product 5 "Tea" 9334.2 "Ooolong" ],
              Purchase (Individual 3 (Person "Poor" "Joe")) [] ]


main :: IO ()
main = runResourceT $
  L.sourceList purchases $$ L.map purchaseToText =$= L.map (LT.toStrict . B.toLazyText)
    =$= L.concatMap (\x -> [x, "\n"])
    =$= T.encode T.utf8 =$ CB.sinkFile "test.xxx"
