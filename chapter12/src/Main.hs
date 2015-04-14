{-# LANGUAGE QuasiQuotes, OverloadedStrings, ScopedTypeVariables,
             RecordWildCards, TemplateHaskell, MultiParamTypeClasses,
             FlexibleContexts#-}

import Prelude hiding (product)

import Data.Text.Lazy (pack)

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger

import Data.Monoid (mconcat)
import Web.Scotty (scotty, Param, get, post, html, notFound, redirect, params, status, param)
import Network.HTTP.Types

import qualified Database.Persist.Sqlite as Db
import qualified Database.Persist.Class
import Chapter12.Database

import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text (Text)
import qualified Data.Text.Lazy as T (toStrict)
-- import Data.Text.Read

import Text.Digestive
import Text.Digestive.Util

import qualified Text.Blaze.Html5 as H
import Text.Digestive.Blaze.Html5

import GHC.Int (Int64)
import Data.Maybe (fromJust)

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

-- data CountryType = China | Japan | USA deriving (Show, Eq, Ord, Read)
-- data ClientProxy = ClientProxy String String String CountryType Int deriving (Show, Eq, Ord)

-- xxx :: Monad m => Formlet String m (Db.Key Country)
-- xxx (Just x) = CountryKey <$> Db.SqlBackendKey x <*> CountryKey
data Thing = ThingOne | ThingTwo
           deriving (Show, Eq)

data XXX = XXX { thething :: Thing, number :: Int } deriving (Show, Eq)

thingForm :: (Monad m) => Form Text m XXX
thingForm = XXX <$> "thething"  .: choice [(ThingOne, "t1"), (ThingTwo, "t2")] Nothing
                <*> "number"    .: stringRead "fail to parse number" Nothing

thingView :: View H.Html -> H.Html
thingView view = do
  form view "/thing" $ do
    label "thething" view "The Thing:"
    inputSelect "thething" view

foo :: String
foo = "Foo"

bar :: String
bar = "Foo"

clientForm :: Monad m => Form String m Client
clientForm = Client <$> "firstName" .: string Nothing 
                         <*> "lastName"  .: string Nothing
                         <*> "address"   .: string Nothing
                         -- <*> "country"   .: stringRead "Cannot parse country" Nothing
                         -- <*> (CountryKey . Db.SqlBackendKey) <$> stringRead "xxx" Nothing
                         -- (CountryKey . Db.SqlBackendKey) <$> stringRead "xxx" Nothing
                         -- <*> (Db.toSqlKey <$> "country" .: stringRead "xxx" Nothing)
                         <*> (Db.toSqlKey <$> "country" .: choice [(1, "USA"), (2, "CHINA")] Nothing)
                         -- <*> (Db.toSqlKey <$> "country" .: choice [(1, "USA"), (2, "CHINA")] Nothing)
                         <*> "age"       .: optionalStringRead "Cannot parse age" Nothing


clientView :: View H.Html -> H.Html
clientView view = do
  form view "/register" $ do
    label "firstName" view "First Name:"
    inputText "firstName" view
    errorList  "firstName"    view
    H.br
    label "lastName" view "Last Name:"
    inputText "lastName" view
    errorList  "lastName"    view
    H.br
    label "address" view "Address:"
    inputText "address" view
    errorList  "address"    view
    H.br
    label "country" view "Country:"
    -- inputRadio True "country" view
    inputSelect "country" view
    errorList  "country"    view
    H.br
    label "age" view "Age:"
    inputText "age" view
    errorList  "age"    view
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

      get "/thing" $ do
        view <- getForm "thing" thingForm
        let view' = fmap H.toHtml view
        html $ renderHtml $
          H.html $ do
            H.head $ H.title "Thing"
            H.body $ thingView view'

      get "/client/:clientId" $ do
        (clientId :: Integer) <- param "clientId"
        client <- liftIO $ flip Db.runSqlPersistMPool pool $
                    Db.get $ Db.toSqlKey (fromIntegral clientId)
        case client of
          Just (Client {..}) -> html $ mconcat [ "<html><body>"
                                               , "  <h1>"
                                               , pack clientFirstName
                                               , "  </h1>"
                                               , "  <h1>"
                                               , pack clientLastName
                                               , "  </h1>"
                                               , "  <h1>"
                                               , pack $ show $ Db.unSqlBackendKey $ unCountryKey $ clientCountry
                                               , "  </h1>"
                                               , "  <h1>"
                                               , pack $ show clientAge
                                               , "  </h1>"
                                               , "</html></body>" ]
          Nothing -> do
                       status notFound404
                       html "<h1> Not found</h1>"


      get "/register" $ do
        view <- getForm "client" clientForm
        let view' = fmap H.toHtml view
        html $ renderHtml $
          H.html $ do
            H.head $ H.title "New Client"
            H.body $ clientView view'

      post "/register" $ do
        params' <- params
        (view, client) <- postForm "client" clientForm (\_ -> return (paramsToEnv params'))
        case client of
          Just c -> do
            key <- liftIO $ Db.runSqlPersistMPool (Db.insert c) pool
            let newId = Db.fromSqlKey key
            redirect $ mconcat ["/client/", pack $ show newId]
          Nothing -> do
            let view' = fmap H.toHtml view
            html $ renderHtml $
              H.html $ do
                H.head $ H.title "Register client"
                H.body $ clientView view'

      notFound $ do
        status notFound404
        html "<h1>Not found :(</h1>"

paramsToEnv :: Monad m => [Param] -> Env m
paramsToEnv [] _ = fail "Parameter not found"
paramsToEnv ((k,v):rest) t = if T.toStrict k == fromPath t
                               then return [TextInput $ T.toStrict v]
                               else paramsToEnv rest t
