module Main where

import Control.Monad (forever)
import Models.Room.Room (Room(..), Direction(..), Exit(..), getExitAtDirection, charToDirection)
import Models.Room.Db (getRoom)

-- Do we need a state monad to save the current position fo the player?

-- getInitialRoom
-- put player in initialRoom

pingpong :: Room -> IO()
pingpong room = do
  print room
  a <- getChar
  handleDirections a
  nextRoom <- getRoomFromDirection room a
  pingpong nextRoom


getRoomFromDirection :: Room -> Char -> IO Room
getRoomFromDirection room dir =
  case getExitAtDirection room  $ charToDirection dir of
    Just (Exit _ roomID) -> getRoom roomID
    Nothing -> getRoom (uid room)

handleDirections :: Char -> IO()
handleDirections a =
  case a of
    'n' -> putStrLn ": you go north"
    's' -> putStrLn ": you go south"
    'e' -> putStrLn ": you go east"
    'w' -> putStrLn ": you go west"
    _ -> putStrLn ": Sorry, I don't understand you"

main :: IO()
main = do
  initialRoom <- getRoom 2
  pingpong initialRoom


-- {-# LANGUAGE OverloadedStrings #-}
--
-- module Main where
--
-- import Web.Scotty
-- import Control.Monad.IO.Class
--
-- import Db
--
-- getUserHandler :: ActionM()
-- getUserHandler = do
--   id <- param "id"
--   user <- liftIO (Db.getUser id) -- liftIO to actionM
--   json user
--
-- getAllUsersHandler :: ActionM()
-- getAllUsersHandler = do
--   users <- liftIO Db.getAllUsers -- liftIO to actionM
--   json users
--
-- createRoom :: ActionM()
-- createRoom = do
--   room <- params
--   text (room)
--
-- getFirstParam :: Param -> String
-- getFirstParam param = (head Param)
--
-- routes :: ScottyM ()
-- routes = do
--   get "/users" getAllUsersHandler
--   get "/users/:id" getUserHandler
--   post "/rooms/create" createRoom
--
-- main :: IO ()
-- main = do
--   putStrLn "Starting Server..."
--   scotty 4888 routes
