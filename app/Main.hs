module Main where

import Models.Room.Room (Room(..), Direction(..), navigateToRoom, getRoom)
import Control.Monad()
import Data.Maybe ()

-- create server / client communication

data State = State {
  room :: Room,
  hp :: Integer,
  roomPath :: [Integer]
} deriving (Eq)

data Action = String | NoAction

createShell :: IO String
createShell = do
  putStr "-> "
  getLine

handleCommands :: Maybe State -> IO ()
handleCommands state =
  case state of
    Just s' -> do
      printStatus s' -- make this only if we have changed rooms or something?
      input <- createShell
      newState <- updateState s' input
      handleCommands newState
    Nothing ->
      --Try to restore state from DB? Do not crash!
      print "Fatal Error"

printStatus :: State -> IO ()
printStatus state = do
  _ <- putStrLn ""
  _ <- putStrLn $ title (room state)
  _ <- putStrLn $ "\x1b[32m" ++ "Exits: " ++ roomExitsToString (room state) ++ "\x1b[0m" ++ "\n"
  _ <- putStrLn $ description (room state)
  pure ()

maybeUpdateRoom :: State -> Maybe Room -> Maybe State
maybeUpdateRoom st = fmap stateFromRoom
    where stateFromRoom :: Room -> State
          stateFromRoom r = st
            { room = r
            , roomPath = roomPath st ++ [uid r]
            }

updateState :: State -> String -> IO (Maybe State)
updateState state actions = do
  let r = room state
  -- [x, y] <- sequence $ fmap (navigateToRoom r) [N, S]
  [x, y] <- traverse (navigateToRoom r) [N, S]

  xr <- pure $ maybeUpdateRoom state x
  yr <- pure $ maybeUpdateRoom state y

  case actions of
    "n" -> pure xr
    "s" -> pure yr
    _ -> pure Nothing -- This crashes and it shouldn't ... what to do when not recognised command?

getRoomDescription :: Room -> String
getRoomDescription room' = "" ++ title room' ++ " " ++ roomExitsToString room' ++ " " ++ description room'

roomExitsToString :: Room -> String
roomExitsToString room' = head (map show (exits room'))

-- getRoomFromDirection :: Room -> Char -> IO Room
-- getRoomFromDirection room dir =
--   case getExitAtDirection room  $ charToDirection dir of
--     Just (Exit _ roomID) -> getRoom roomID
--     Nothing -> nothingRoom
--     where nothingRoom = getRoom 4

handleDirections :: Char -> IO()
handleDirections a =
  case a of
    'n' -> putStrLn ": you are headed north"
    's' -> putStrLn ": you are headed south"
    'e' -> putStrLn ": you are headed east"
    'w' -> putStrLn ": you are headed west"
    _ -> putStrLn ": Sorry, I don't understand you"

createInitialState :: IO State
createInitialState = do
  initialRoom <- getRoom 2
  pure State {
      room = initialRoom
    , hp = 10
    , roomPath = []
  }

main :: IO()
main = do
  initialState <- createInitialState
  handleCommands $ Just initialState

-- Scotty server example

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
