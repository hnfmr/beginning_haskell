-- ex 10.3
import DataBuilder
import Json

-- data Product = Product { id :: Int, name :: String, price :: Double, descr :: String } deriving (Show, Ord, Eq)

-- data Purchase = Purchase { client :: Client Int, products :: [Product] } deriving (Show, Ord, Eq)
import Data.Conduit
import qualified Data.Conduit.Binary as B
-- import qualified Data.Conduit.Attoparsec as A
-- import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as L
import qualified Data.ByteString as LB
import qualified Data.ByteString.Lazy as LLB
 
import Control.Monad.Trans.Resource(runResourceT)
import Data.Aeson
-- import Data.Text

savePurchases :: FilePath -> [Purchase] -> IO ()
savePurchases fPath purchases = runResourceT $
  yield (toJSON purchases) $$ L.map (LLB.toStrict . encode) =$ B.sinkFile fPath

purchases :: [Purchase]
purchases = [ Purchase (GovOrg 1 "MoE") [ Product 11 "Soap" 1234.2 "Bubbly...", Product 22 "TV" 29394.2 "SONY" ],
              Purchase (Individual 2 (Person "John" "Huges")) [ Product 33 "Tester" 1.23 "Awe", Product 44 "Car" 2993.4 "Kia" ] ]

displayP :: Monad m =>  Conduit LB.ByteString m [Purchase]
displayP = do
  t <- await
  case t of
    Nothing -> return ()
    Just c -> do
      let x = decodeStrict c :: (Maybe [Purchase])
      case x of
        Nothing -> return ()
        Just y -> do
          yield y
          displayP

-- FIXME TODO
-- List of list is pretty ugly
showPurchases :: IO [[Purchase]]
showPurchases = runResourceT $
  B.sourceFile "test.db" $$ B.lines =$ displayP =$ L.consume
  
main :: IO ()
-- main = savePurchases "test.db" purchases >> showPurchases
main = do
  savePurchases "test.db" purchases
  ps <- showPurchases
  let pps = Prelude.concat ps
  print pps
