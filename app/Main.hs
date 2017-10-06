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
    states :: [State]
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
  let currentState' = currentState app
      states' = states app in
        if currentState' /= last states'
          then app { states = states' ++ [currentState']}
          else app

-- Split this into pieces
handleCommands :: App -> IO ()
handleCommands app = do
  _ <- handleRoomUpdate app -- Send back info to the client
  newApp' <- pure $ stackState app -- Push the stack
  input <- createShell -- Wait for inut
  newApp <- updateStateRoom newApp' input -- Handle input to update app
  handleCommands newApp -- Begin Again

handleRoomUpdate :: App -> IO ()
handleRoomUpdate app =
  if currentRoom /= lastRoom
    then printStatus app
    else pure ()
  where currentRoom = room (currentState app)
        lastRoom = room $ last $ states app

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
  handleCommands App { states = [initialState] , currentState = initialState }
