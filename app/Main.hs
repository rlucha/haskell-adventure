module Main where

import Models.Room.Room (Room(..), Direction(..), navigateToRoom, getRoom)
import Control.Monad()
import Data.Maybe()

-- create server / client communication

data State = State {
  room :: Room,
  hp :: Integer,
  stateHistory :: Maybe [State]
} deriving (Eq, Show)

-- create an instance of Eq and ignore stateHistory? Or maybe create a new Array out of State and check previous and current...


data Action = String | NoAction

createShell :: IO String
createShell = do
  putStr "-> "
  getLine


stackState :: State -> State -> State
stackState state nextState = case stateHistory state of
  Just st' -> state { stateHistory = Just $ st' ++ [nextState]}
  Nothing -> state { stateHistory = Just [nextState]}

handleCommands :: State -> IO ()
handleCommands state =
  -- If you don't have  a stateHistory, we treat as initialState
  case stateHistory state of
    Nothing -> do -- This is just for the initial Program case
      _ <- printStatus state
      newState <- pure $ stackState state state
      handleCommands newState
    Just sth -> do
      isNewRoom <- pure $ room (last sth) /= room state
      if isNewRoom
        then do
          _ <- print "You move from room"
          _ <- printStatus state
          input <- createShell
          newState <- updateState state input
          newState' <- pure $ stackState state newState
          handleCommands newState'
        else do
          input <- createShell
          newState <- updateState state input
          handleCommands newState


-- -- Handle the maybe state for the initial case (fromMaybe?)
-- handleCommands :: State -> State -> IO ()
-- handleCommands state nextState =
--   case nextState of
--     EmptyState -> do
--       -- fetch the state from the last stateHistory item
--       _ <- print nextState
--       lastGoodState <- pure $ last (stateHistory state)
--       input <- createShell
--       newState <- updateState lastGoodState input
--       handleCommands lastGoodState newState
--     State {} ->  --first time we might have an EmptyState here... we don't want to do that
--       case state of
--         State {} -> do
--           st' <- pure $ state { stateHistory = stateHistory state ++ [nextState] }
--           _ <- printStatus nextState
--           input <- createShell
--           newState <- updateState st' input
--           handleCommands state newState
--         EmptyState -> do
--           st' <- pure nextState
--           _ <- printStatus nextState
--           input <- createShell
--           newState <- updateState st' input
--           handleCommands state newState


printStatus :: State -> IO ()
printStatus state = do
  _ <- putStrLn ""
  _ <- putStrLn $ title (room state)
  _ <- putStrLn $ "\x1b[32m" ++ "Exits: " ++ roomExitsToString (room state) ++ "\x1b[0m" ++ "\n"
  _ <- putStrLn $ description (room state)
  pure ()


updateRoom :: State -> Maybe Room -> State
updateRoom st r = case r of
    Just r' -> st { room = r' }
    Nothing -> st

updateState :: State -> String -> IO State
updateState state actions = do
  let r = room state
  -- [x, y] <- sequence $ fmap (navigateToRoom r) [N, S]
  [x, y] <- traverse (navigateToRoom r) [N, S]

  xr <- pure $ updateRoom state x
  yr <- pure $ updateRoom state y

  case actions of
    "n" -> pure xr
    "s" -> pure yr
    _ -> pure state

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
    , stateHistory = Nothing
  }

main :: IO()
main = do
  initialState <- createInitialState
  handleCommands initialState

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
