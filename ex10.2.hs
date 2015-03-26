{-# LANGUAGE OverloadedStrings #-}

import DataBuilder

import Control.Applicative
import Data.Attoparsec.Text

aChar :: Parser Char
aChar =     (const ',') <$> (string "\\,") <|> (const '\n') <$> (string "\\n")
        <|> (const '(') <$> (string "\\(") <|> (const ')')  <$> (string "\\)")
        <|> satisfy (notInClass ",\n()")

aString :: Parser String
-- aString = ((:) <$> aChar <*> aString) <|> (pure "")
aString = many aChar

aPerson :: Parser Person
aPerson = Person <$ string "person(" <*> aString <* char ',' <*> aString <* char ')'

aClient :: Parser (Client Int)
aClient =    GovOrg     <$ string "client(gov," <*> decimal <* char ',' <*> aString <* char ')'
         <|> Company    <$ string "client(com," <*> decimal <* char ',' <*> aString <* char ','
                        <*> aPerson <* char ',' <*> aString <* char ')'
         <|> Individual <$ string "client(ind," <*> decimal <* char ',' <*> aPerson <* char ')'
{-
data Person = Person { firstName :: String, lastName :: String }
              deriving (Show, Ord, Eq)

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Ord, Eq)

data Product = Product { id :: Int, name :: String, price :: Double, descr :: String } deriving (Show, Ord, Eq)

data Purchase = Purchase { client :: Client Int, products :: [Product] } deriving (Show, Ord, Eq)
-}

aProduct :: Parser Product
-- aProduct = Product <$ string "product(" <*> decimal <* char ',' <*> aString <* char ',' <*> double <* char ','
--                    <*> aString <* char ')'
aProduct = do
  string "product("
  id <- decimal
  char ','
  name <- aString
  char ','
  price <- double
  char ','
  descr <- aString
  char ')'
  return $ Product id name price descr

