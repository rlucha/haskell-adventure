{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db where

import GHC.Generics
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Aeson (FromJSON, ToJSON)

data GameUser = GameUser String String
  deriving (Show, Generic)

instance ToJSON GameUser
instance FromJSON GameUser

-- getGameUserDetails :: GameUser -> String
-- getGameUserDetails (GameUser username email) = show username ++ show email

sqlToGameUser::[SqlValue] -> GameUser
sqlToGameUser row = GameUser (fromSql $ head row) (fromSql $ row!!1)

getAllUsers:: IO [GameUser]
getAllUsers = do
  -- this path is absolute to the root of the project it seems
  conn <- connectSqlite3 "db.sql"
  q <- quickQuery' conn "select * from users" []
  return (map sqlToGameUser q)
