{-# LANGUAGE QuasiQuotes, OverloadedStrings, ScopedTypeVariables, RecordWildCards, TemplateHaskell #-}

import Prelude hiding (product)

import Data.Text.Lazy (pack)

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger

import Data.Monoid (mconcat)
import Web.Scotty
import Network.HTTP.Types

import qualified Database.Persist.Sqlite as Db
-- import qualified Database.Persist.Class
import Chapter12.Database

import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text (Text)

import Text.Digestive

data MyRoute = Products | About

render :: MyRoute ->[(Text, Text)] -> Text
render About    _ = "/about"
render Products _ = "/products"

-- footer ::HtmlUrl MyRoute
-- footer = [hamlet|
-- <footer>
--   Return to #
--   <a href=@{Home}>Homepage
--   .
-- |]

main :: IO ()
main = do
  Db.runSqlite "example.db" $ Db.runMigration migrateAll
  runNoLoggingT $ Db.withSqlitePool "example.db" 10 $ \pool -> NoLoggingT $
    scotty 3000 $ do
      get "/about" $
        html $ mconcat [ "<html><body>"
                       , "  <h1>Hello Beginning Haskell!</h1>"
                       , "</body></html>" ]
      get "/products" $ do
        (products :: [Db.Entity Product]) <-
          liftIO $ flip Db.runSqlPersistMPool pool $ Db.selectList [] []
      
        html $ renderHtml $ [hamlet|
          <table>
            <tr>
              <th>Name
              <th>Description
            $forall Db.Entity _ p <- products
              <tr>
                <td>#{productName p}
                <td>#{productDescription p}
        |] render
      get "/product/:productId" $ do
        (productId :: Integer) <- param "productId"
        product <- liftIO $ flip Db.runSqlPersistMPool pool $
                     -- get $ Key (Db.PersistInt64 $ fromIntegral productId)
                     Db.get $ Db.toSqlKey (fromIntegral productId)
        case product of
          Just (Product {..}) -> html $ mconcat [ "<html><body>"
                                                , "  <h1>"
                                                , pack productName
                                                , "  </h1>"
                                                , "  <p>"
                                                , pack productDescription
                                                , "  </p>"
                                                , "</html></body>" ]
          Nothing             -> do
                                   status notFound404
                                   html "<h1>Not found :( product</h1>"
      notFound $ do
        status notFound404
        html "<h1>Not found :(</h1>"

-- main = putStrLn $ renderHtml $ [hamlet|
-- <body>
--   <p>This is my page.
--   ^{footer}
-- |] render
