{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mconcat)
import Web.Scotty
import Network.HTTP.Types

import qualified Database.Persist.Sqlite as Db
import chapter12.Database

main :: IO ()
main = scotty 3000 $ do
  get "/about" $
    html $ mconcat [ "<html><body>"
                   , "  <h1>Hello Beginning Haskell!</h1>"
                   , "</body></html>" ]
  notFound $ do
    status notFound404
    html "<h1>Not found :(</h1>"
