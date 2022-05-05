-- Main.hs 

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
    (decodeUtf8, encodeUtf8)

import Data.Typeable
import Database.SQLite.Simple
    hiding (close)
import qualified Database.SQLite.Simple
    as SQLite

import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv)
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import Network.Socket.ByteString
    (recv, sendAll)
import Text.RawString.QQ

data Auto =
    Auto {
        autoId :: Integer 
      , year :: Integer
      , makeandmodel :: Text
      , mileage :: Integer 
      , color :: Text 
      , engine :: Text
      , price :: Integer       
    } deriving (Eq, Show) 

instance FromRow Auto where
    fromRow = Auto <$> field    -- autoId
                   <*> field    -- year
                   <*> field    -- makeandmodel
                   <*> field    -- mileage
                   <*> field    -- color
                   <*> field    -- engine 
                   <*> field    -- price

instance ToRow Auto where
  toRow (Auto id_ year makeandmodel mileage color engine price) =
    toRow (id_, year, makeandmodel, mileage, color, engine, price) 

createAutos :: Query
createAutos = [r|
CREATE TABLE IF NOT EXISTS autos
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
    year INTEGER PRIMARY KEY,
    makeandmodel TEXT,
    mileage INTEGER PRIMARY KEY,
    color TEXT, 
    engine TEXT,
    price INTEGER PRIMARY KEY)
|]

insertAuto :: Query         -- for inserting a new Auto
insertAuto =
  "INSERT INTO autos\
  \ VALUES (?, ?, ?, ?, ?, ?, ?)"

allAutos :: Query           -- for getting all Autos from the user table
allAutos =
  "SELECT * from autos"

getMakeAndModelQuery :: Query   --getting all the fields for a make and model
getMakeAndModelQuery =
  "SELECT * from autos where makeandmodel = ?" 

getMinimumYearQuery :: Query       
getMinimumYearQuery =
  "SELECT * from autos where year >= ?"      -- try >=

getMaximumMileageQuery :: Query       
getMaximumMileageQuery =
  "SELECT * from autos where mileage <= ?"   -- try <= 

getColorQuery :: Query       
getColorQuery =
  "SELECT * from autos where color = ?"   

getEngineQuery :: Query       
getEngineQuery =
  "SELECT * from autos where engine = ?"

getMaximumPriceQuery :: Query       
getMaximumPriceQuery =
  "SELECT * from autos where price <= ?"

type AutoRow =  -- AutoRow is a type synonym for the tuples we insert to create a new Auto
  (Null, Integer, Text, Integer, Text, Text, Integer)

getAuto :: Connection
        -> Text
        -> IO (Maybe Auto)
getAuto conn makeandmodel = do
  results <-
    query conn getAutoQuery (Only makeandmodel)

case results of
  [] -> return $ Nothing
  [auto] -> return $ Just auto

createDatabase :: IO () 
createDatabase = do 
  conn <- open "finger.db" 
  execute_ conn createUsers 
  execute conn insertUser meRow 

  rows <- query_ conn allAutos 
  mapM_ print (rows :: [Auto]) 
  SQLite.close conn 

main :: IO () 
main = createDatabase 

