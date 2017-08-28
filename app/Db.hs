{-# LANGUAGE OverloadedStrings #-}

module Db where

import Database.HDBC
import Database.HDBC.Sqlite3

test = do
  -- this path is absolute to the root of the project it seems
  conn <- connectSqlite3 "db.sql"
  q    <- quickQuery' conn "select * from users" []
  print q
