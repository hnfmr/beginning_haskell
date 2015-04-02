-- ex 10.3
import DataBuilder
import Json

-- data Product = Product { id :: Int, name :: String, price :: Double, descr :: String } deriving (Show, Ord, Eq)

-- data Purchase = Purchase { client :: Client Int, products :: [Product] } deriving (Show, Ord, Eq)
import Data.Conduit
import qualified Data.Conduit.Binary as B

main :: IO ()
main = print (Person "f" "l")
