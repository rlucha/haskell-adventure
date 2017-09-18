{-# LANGUAGE OverloadedStrings #-}

module Models.Room.Db where

import Database.HDBC
import Database.HDBC.Sqlite3

import Models.Room.Room (Room(..), Exit(..))

sqlToRoom :: [SqlValue] -> Room
sqlToRoom row = Room (fromSql $ row!!1) (fromSql $ row!!2)

getRooms :: IO [Room]
getRooms = do
  conn <- connectSqlite3 "db.sql"
  q <- quickQuery' conn "select * from rooms" []
  return $ map sqlToRoom q

getRoom :: Integer -> IO Room
getRoom id = do
  conn <- connectSqlite3 "db.sql"
  q <- quickQuery' conn "select * from rooms where id = ?" [toSql id]
  return $ head (map sqlToRoom q)

-- how to handle errors?
createRoom :: Room -> IO ()
createRoom room = do
  conn <- connectSqlite3 "db.sql"
  stmt <- prepare conn "insert into rooms (name, description) values (?,?)"
  execute stmt [toSql (name room), toSql (description room)]
  commit conn
  disconnect conn
