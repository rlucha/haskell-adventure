{-# LANGUAGE OverloadedStrings #-}

module Db where

import Database.HDBC
import Database.HDBC.Sqlite3

data GameUser = GameUser String String
  deriving (Show)

sqlToGameUser::[SqlValue] -> GameUser
sqlToGameUser row = GameUser (fromSql $ head row) (fromSql $ row!!1)

getAllUsers:: IO [GameUser]
getAllUsers = do
  -- this path is absolute to the root of the project it seems
  conn <- connectSqlite3 "db.sql"
  q <- quickQuery' conn "select * from users" []
  return (map sqlToGameUser q)
