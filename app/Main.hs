-- TODO
-- Create server / client communication
-- Use handleDirections to print player direction
-- Move terminal output fns to own file
-- Add creation of room from terminal

module Main where

import Models.Room.Room (Room(..), Direction(..), navigateToRoom, getRoom)
import Control.Monad()
import Data.Maybe()

data App = App {
    states :: Maybe [State]
  , currentState :: State
} deriving (Show)

data State = State {
  room :: Room,
  hp :: Integer
} deriving (Eq, Show)

createShell :: IO String
createShell = do
  putStr "-> "
  getLine

-- Appends the currentState to the app states history
-- if different from the previous one
stackState :: App -> App
stackState app =
  let currentState' = currentState app in
    case states app of
      Just states' ->
        if currentState' /= last states'
          then app { states = Just $ states' ++ [currentState']}
          else app
      Nothing -> app { states = Just [currentState']}

handleCommands :: App -> IO ()
handleCommands app =
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
