{-# LANGUAGE OverloadedStrings #-}

module Models.Room.Db where

import Database.HDBC
import Database.HDBC.Sqlite3

import Models.Room.Room (Room(..), Exit(..), charToDirection)

sqlToRoom :: ([SqlValue], [[SqlValue]]) -> Room
sqlToRoom (row, exits) = Room (fromSql $ row!!1) (fromSql $ row!!2) (map sqlToExit exits) (fromSql roomID)
  where roomID = head row

sqlToExit :: [SqlValue] -> Exit
sqlToExit row = Exit (charToDirection (fromSql $ head row)) (fromSql $ row!!1)

-- getRooms :: IO [Room]
-- getRooms = do
--   conn <- connectSqlite3 "db.sql"
--   q <- quickQuery' conn "select * from rooms" []
--   return $ map sqlToRoom q

getRoom :: Integer -> IO Room
getRoom id = do
  conn <- connectSqlite3 "db.sql"
  q <- quickQuery' conn "select * from rooms where id = ?" [toSql id]
  e <- quickQuery' conn "select direction, exit_room_id from exits where room_id = ?" [toSql id]
  return $ head (map sqlToRoom [(head q, e)]) --this is wrong, bad params

-- how to handle errors?
createRoom :: Room -> IO ()
createRoom room = do
  conn <- connectSqlite3 "db.sql"
  stmt <- prepare conn "insert into rooms (name, description) values (?,?)"
  execute stmt [toSql (name room), toSql (description room)]
  commit conn
  disconnect conn

-- createRoomsExits :: Room -> IO ()
-- createRoomsExits room = do
--   conn <- connectSqlite3 "db.sql"
--   roomID <- quickQuery' conn "select id from rooms where name = ?" [toSql (name room)]
--   stmt <- prepare conn "insert into exits (room_id, north, south, west, east) values (?,?,?,?)" [toSql roomID, (maybe?? exit)]
--   execute stmt [toSql (name room), toSql (description room)]
--   commit conn
--   disconnect conn
