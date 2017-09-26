module Main where

import Models.Room.Room (Room(..), Direction(..), Exit(..), getExitAtDirection, charToDirection)
import Models.Room.Db (getRoom)

-- create server / client communication
-- Move between rooms from DB

data State = State {
  room :: Room,
  hp :: Integer
}

handleCommands :: State -> IO ()
handleCommands state = do
  showStatus state
  putStr "-> "
  input <- getLine
  actions <- createActions input
  newState <- pure (updateState state actions)
  handleCommands newState

showStatus :: State -> IO ()
showStatus state = putStrLn $ description (room state)

createActions :: String -> IO ()
createActions input =
  case input of
    "n" -> putStrLn "you go north"
    "s" -> putStrLn "you go south"
    _   -> putStrLn "what?"

updateState :: State -> () -> State
updateState state actions = state

pingpong :: Room -> IO()
pingpong room = do
  print $ getRoomDescription room
  a <- getChar
  handleDirections a
  nextRoom <- getRoomFromDirection room a
  pingpong nextRoom

getRoomDescription :: Room -> String
getRoomDescription room = "" ++ title room ++ " " ++ roomExitsToString room ++ " " ++ description room

roomExitsToString :: Room -> String
roomExitsToString room = head (map show (exits room))

getRoomFromDirection :: Room -> Char -> IO Room
getRoomFromDirection room dir =
  case getExitAtDirection room  $ charToDirection dir of
    Just (Exit _ roomID) -> getRoom roomID
    Nothing -> nothingRoom
    where nothingRoom = getRoom 4

handleDirections :: Char -> IO()
handleDirections a =
  case a of
    'n' -> putStrLn ": you are headed north"
    's' -> putStrLn ": you are headed south"
    'e' -> putStrLn ": you are headed east"
    'w' -> putStrLn ": you are headed west"
    _ -> putStrLn ": Sorry, I don't understand you"

main :: IO()
main = do
  initialRoom <- getRoom 2
  initialState <- pure State { -- move this to function
      room = initialRoom
    , hp = 10
  }
  handleCommands initialState


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
