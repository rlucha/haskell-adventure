module Main where

import Models.Room.Room (Room(..), Direction(..), navigateToRoom, getRoom)
import Control.Monad()
import Data.Maybe()


data App = App {
    states :: Maybe [State]
  , currentState :: State
} deriving (Show)

-- create server / client communication

data State = State {
  room :: Room,
  hp :: Integer
} deriving (Eq, Show)

-- create an instance of Eq and ignore stateHistory? Or maybe create a new Array out of State and check previous and current...


data Action = String | NoAction

createShell :: IO String
createShell = do
  putStr "-> "
  getLine

-- check that state is different before pushing
stackState :: App -> App
stackState app =
  let currentState' = currentState app in
    case states app of
      Just states' ->
        if currentState' /= last states'
          then app { states = Just $ states' ++ [currentState']} -- append only if different states
          else app
      Nothing -> app { states = Just [currentState']}

-- Change this to App, handle array of states instead of internal State history, to make State Eq easy
handleCommands :: App -> IO ()
handleCommands app =
-- If you don't have  a stateHistory, we treat as initialState
  case states app of
    Nothing -> do -- This is just for the initial Program case
      newApp <- pure $ stackState app
      _ <- printStatus newApp
      handleCommands newApp
    Just _ -> do
      _ <- handleRoomUpdate app
      newApp' <- pure $ stackState app
      input <- createShell
      newApp <- updateStateRoom newApp' input
      handleCommands newApp

handleRoomUpdate :: App -> IO ()
handleRoomUpdate app =
  case states app of
    Just states' ->
      if currentRoom /= lastRoom
        then printStatus app
        else pure ()
      where currentRoom = room (currentState app)
            lastRoom = room $ last states'
    Nothing -> pure ()

-- -- Handle the maybe state for the initial case (fromMaybe?)
-- handleCommands :: State -> State -> IO ()
-- handleCommands state nextState =
--   case nextState of
--     EmptyState -> do
--       -- fetch the state from the last stateHistory item
--       _ <- print nextState
--       lastGoodState <- pure $ last (stateHistory state)
--       input <- createShell
--       newState <- updateStateRoom lastGoodState input
--       handleCommands lastGoodState newState
--     State {} ->  --first time we might have an EmptyState here... we don't want to do that
--       case state of
--         State {} -> do
--           st' <- pure $ state { stateHistory = stateHistory state ++ [nextState] }
--           _ <- printStatus nextState
--           input <- createShell
--           newState <- updateStateRoom st' input
--           handleCommands state newState
--         EmptyState -> do
--           st' <- pure nextState
--           _ <- printStatus nextState
--           input <- createShell
--           newState <- updateStateRoom st' input
--           handleCommands state newState


printStatus :: App -> IO ()
printStatus app =
  let currentState' = currentState app in do
    _ <- putStrLn ""
    _ <- putStrLn $ title (room currentState')
    _ <- putStrLn $ "\x1b[32m" ++ "Exits: " ++ roomExitsToString (room currentState') ++ "\x1b[0m" ++ "\n"
    _ <- putStrLn $ description (room currentState')
    pure ()


updateRoom :: App -> Maybe Room -> App
updateRoom app r = case r of
    Just r' -> app { currentState = (currentState app) { room = r' } }
    Nothing -> app

updateStateRoom :: App -> String -> IO App
updateStateRoom app actions = do
  let r = room (currentState app)
  -- [x, y] <- sequence $ fmap (navigateToRoom r) [N, S]
  [x, y] <- traverse (navigateToRoom r) [N, S]

  xr <- pure $ updateRoom app x
  yr <- pure $ updateRoom app y

  case actions of
    "n" -> pure xr
    "s" -> pure yr
    _ -> pure app

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
  }

main :: IO()
main = do
  initialState <- createInitialState
  handleCommands App { states =  Nothing , currentState = initialState }

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
