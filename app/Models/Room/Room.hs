{-# LANGUAGE DeriveGeneric #-}

module Models.Room.Room where

import GHC.Generics
import Control.Monad.IO.Class
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (decode, FromJSON)
import Data.ByteString.Lazy (ByteString)
import Data.List (find)

data Room = Room {
    name :: String
  , title :: String
  , description :: String
  , exits :: [Exit]
  , uid :: Integer
} deriving (Eq, Show, Generic)

instance FromJSON Room

data Direction = N | S | W | E | Back | Invalid
  deriving (Eq, Show, Generic)

instance FromJSON Direction

data Exit = Exit {
  direction :: Direction,
  roomID :: Integer
} deriving (Eq, Generic)

instance FromJSON Exit

instance Show Exit where
  show (Exit d r) = show d

create :: ByteString -> Maybe Room
create = decode

getExitAtDirection :: Room -> Direction -> Maybe Exit
getExitAtDirection room dir = find (\ex -> direction ex == dir) (exits room)

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
