import Data.Conduit
import Person
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.Serialization.Binary as S

import Control.Monad.Trans.Resource (runResourceT)

main :: IO ()
main = runResourceT $ L.sourceList clients $$ S.conduitEncode =$ B.sinkFile "person.db"
  where clients = [ Person "Alejandro" "Serrano", Person "The Doctor" "Who?" ]
