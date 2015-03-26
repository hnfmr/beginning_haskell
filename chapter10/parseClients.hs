{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid((<>))

import Control.Applicative
import Data.Attoparsec.Text

import Data.Text
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B


data Person = Person { firstName :: String, lastName :: String }
              deriving (Show, Ord, Eq)

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Ord, Eq)

data Product = Product { id :: Int, name :: String, price :: Double, descr :: String } deriving (Show, Ord, Eq)

data Purchase = Purchase { client :: Client Int, products :: [Product] } deriving (Show, Ord, Eq)

escapeString :: String -> Text
escapeString = replace "\n" "\\n" . replace "," "\\," . replace "(" "\\(" .
               replace ")" "\\)" . pack

personToText :: Person -> B.Builder
personToText (Person f l) =
  "person(" <> B.fromText (escapeString f) <> B.singleton ','
            <> B.fromText (escapeString l) <> B.singleton ')'
               
clientToText :: Client Int -> B.Builder
clientToText (GovOrg i n) =
  "client(gov," <> B.decimal i <> B.singleton ',' <> B.fromText (escapeString n) <> B.singleton ')'

clientToText (Company i n p d) =
  "client(com," <> B.decimal i <> B.singleton ',' <> B.fromText (escapeString n) <> B.singleton ','
                <> personToText p <> B.singleton ',' <> B.fromText (escapeString d) <> B.singleton ')'

clientToText (Individual i p) =
  "client(ind," <> B.decimal i <> B.singleton ',' <> personToText p <> B.singleton ')'


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
