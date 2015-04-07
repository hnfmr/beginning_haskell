{-# LANGUAGE QuasiQuotes, OverloadedStrings, ScopedTypeVariables, RecordWildCards, TemplateHaskell, MultiParamTypeClasses #-}

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
import Data.Text.Lazy (toStrict)

import Text.Digestive
import Text.Digestive.Util

import qualified Text.Blaze.Html5 as H
import Text.Digestive.Blaze.Html5

data MyRoute = Products | About

render :: MyRoute ->[(Text, Text)] -> Text
render About    _ = "/about"
render Products _ = "/products"

countryForm :: Monad m => Form String m Country
countryForm = Country <$> "name" .: string Nothing
                      <*> "send" .: bool (Just True)

productForm :: Monad m => Form String m Product
productForm = Product <$> "name"        .: string Nothing
                      <*> "description" .: string Nothing
                      <*> "price"       .: validate isANumber (string Nothing)
                      <*> "inStock"     .: check "Must be >= 0" (>= 0)
                                             (validate isANumber (string Nothing))

isANumber :: (Num a, Read a) => String -> Result String a
isANumber = maybe (Error "Not a number") Success . readMaybe

productView :: View H.Html -> H.Html
productView view = do
  form view "/new-product" $ do
    label      "name"     view "Name:"
    inputText  "name"     view
    H.br
    label      "description" view "Descriptipn:"
    inputTextArea Nothing Nothing "description" view
    H.br
    label      "price"    view "Price:"
    inputText  "price"    view
    errorList  "price"    view
    H.br
    label      "inStock"  view "# in Stock:"
    inputText  "inStock"  view
    errorList  "inStock"  view
    H.br
    inputSubmit "Submit"

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
      get "/new-product" $ do
        view <- getForm "product" productForm
        let view' = fmap H.toHtml view
        html $ renderHtml $
          H.html $ do
            H.head $ H.title "Grocery Store"
            H.body $ productView view'

      post "/new-product" $ do
        params' <- params
        (view, product) <- postForm "product" productForm (\_ -> return (paramsToEnv params'))
        case product of
          Just p -> do
            key <- liftIO $ Db.runSqlPersistMPool (Db.insert p) pool
            let newId = Db.fromSqlKey key
            redirect $ mconcat ["/product/", pack $ show newId]
          Nothing -> do
            let view' = fmap H.toHtml view
            html $ renderHtml $
              H.html $ do
                H.head $ H.title "Grocery Store"
                H.body $ productView view'

      notFound $ do
        status notFound404
        html "<h1>Not found :(</h1>"

paramsToEnv :: Monad m => [Param] -> Env m
paramsToEnv [] _ = fail "Parameter not found"
paramsToEnv ((k,v):rest) t = if toStrict k == fromPath t
                               then return [TextInput $ toStrict v]
                               else paramsToEnv rest t
