{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Room.Room where

import GHC.Generics
import Control.Monad.IO.Class()
import Control.Applicative ((<$>))
import Data.Aeson (decode, FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.List (find)
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe

data Room = Room {
    uid :: Integer
  , name :: String
  , title :: String
  , description :: String
  , exits :: [Exit] --Make this maybe?
} deriving (Eq, Show, Generic)

instance FromJSON Room
instance ToJSON Room

data Direction = N | S | W | E | Back | Invalid
  deriving (Eq, Show, Generic)

instance FromJSON Direction
instance ToJSON Direction

data Exit = Exit {
  direction :: Direction,
  roomID :: Integer
} deriving (Eq, Generic)

instance FromJSON Exit
instance ToJSON Exit

instance Show Exit where
  show (Exit d _) = show d

create :: ByteString -> Maybe Room
create = decode

-- getExitAtDirection :: Room -> Direction -> Maybe Exit
-- getExitAtDirection room dir = find (\ex -> direction ex == dir) (exits room)

navigateToRoom :: Room -> Direction -> IO (Maybe Room) -- Turn this around and make it a IO (Maybe Room)
navigateToRoom room dir =
  sequence $ case find (\exit-> direction exit == dir) (exits room) of
    Just e -> Just (getRoom $ roomID e)
    Nothing -> Nothing

-- pass this to mapLookup using fetchRowsMap(?)
sqlToRoom :: ([SqlValue], [[SqlValue]]) -> Room
sqlToRoom (row, exits') = Room roomID' name' title' description' mapExits
  where roomID' = fromSql $ head row
        name' = fromSql $ row!!1
        title' = fromSql $ row!!2
        description' = fromSql $ row!!3
        mapExits = catMaybes $ sqlToExit <$> exits'

sqlToExit :: [SqlValue] -> Maybe Exit
sqlToExit row = case row of
  [] -> Nothing -- is this the proper type for empty SqlValue ?
  _ -> Just $ Exit (charToDirection (fromSql $ head row)) (fromSql $ row!!1)

-- getRooms :: IO [Room]
-- getRooms = do
-- get all the rooms IDs
-- map those IDs over getRoom

-- getRooms :: IO [Room]
-- getRooms = do
--   conn <- connectSqlite3 "db.sql"
--   q <- quickQuery' conn "select * from rooms" []
--   return $ map sqlToRoom q

-- change this to fechtRowAl or fetchRowMap ?
-- move the connection to a shared space?
getRoom :: Integer -> IO Room
getRoom id' = do
  conn <- connectSqlite3 "db.sql"
  q <- quickQuery' conn "select * from rooms where id = ?" [toSql id']
  e <- quickQuery' conn "select direction, exit_room_id from exits where room_id = ?" [toSql id']
  disconnect conn
  return $ sqlToRoom (head q, e) -- avoid head q, make it headMay

createRoom :: String -> String -> String -> IO ()
createRoom name' desc' title' = do
  conn <- connectSqlite3 "db.sql"
  stmt <- prepare conn "insert into rooms (name, description, title) values (?, ?, ?)"
  _    <- execute stmt [toSql name', toSql desc', toSql title']
  commit conn
  disconnect conn

createExit :: Room -> Direction -> roomID -> IO ()
createExit room' dir' exitRoomID' = do
  conn <- connectSqlite3 "db.sql"
  stmt <- prepare conn "insert into exits (room_id, direction, exit_room_id) values (?, ?, ?)"
  _    <- execute stmt [toSql (uid room'), toSql dir', toSql exitRoomID']
  commit conn
  disconnect conn

-- class FromRow a where
--   readRow :: SqlValue -> a

-- instance FromRow Room where
--   readRow sqlValue = Room
--     { name = fromSql $ row !! 1
--     , description = fromSql $ row !! 2
--     , exits = absurd
--     , uid = Nothing
--     }
--
--  instance FromRow Exit where
--    readRow sqlValue = Exit
--     {
--
--     }
-- getRoomMap :: Integer -> IO Room
-- getRoomMap id = do
--   conn <- connectSqlite3 "db.sql"
--   stmt <- prepare conn "select * from rooms where id = 2"
--   room <- fetchAllRowsMap' (execute stmt []) [Map String SqlValue]
--   print room

-- this returns [Map String SqlValue]
-- extract it with ...
-- extractPatternStrings ∷ [Map String SqlValue] → [String]
-- extractPatternStrings []     = []
-- extractPatternStrings (m:ms) = toString m : extractPatternStrings ms
--     where
--         toString ∷  Map String SqlValue → String
--         toString m = (fromSql . fromJust . (Map.lookup "word"))∷ String

-- createRoomsExits :: Room -> IO ()
-- createRoomsExits room = do
--   conn <- connectSqlite3 "db.sql"
--   roomID <- quickQuery' conn "select id from rooms where name = ?" [toSql (name room)]
--   stmt <- prepare conn "insert into exits (room_id, north, south, west, east) values (?,?,?,?)" [toSql roomID, (maybe?? exit)]
--   execute stmt [toSql (name room), toSql (description room)]
--   commit conn
--   disconnect conn


-- replace fromSql call with a typeclass instance?
charToDirection :: Char -> Direction
charToDirection d = case d of
  'n' -> N
  's' -> S
  'e' -> E
  'w' -> W
  'b' -> Back
  _ -> Invalid

-- How to load json from files and create data from it
-- createRoom :: B.ByteString -> Maybe Room
-- createRoom = decode
--
-- -- parseRoom :: String -> Maybe Room
-- -- parseRoom file = (B.readFile file) >>= decode
--
-- -- parseArea :: Folder name -> Area :: [Rooms]
-- -- map all rooms on resources and create in memory rooms
-- -- because of the structure of the exits it will create a room graph
--
-- -- "resources/room.json"
-- test :: String -> IO ()
-- test file = do
--   fileIN <- B.readFile file
--   let room = createRoom fileIN
--   case room of
--     Just r -> print (name r)
--     Nothing -> print "error"
