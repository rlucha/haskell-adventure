-- TODO
-- Create server / client communication
-- Use handleDirections to print player direction
-- Move terminal output fns to own file
-- Add creation of room from terminal

{-# LANGUAGE MultiWayIf#-}
{-# LANGUAGE OverloadedStrings#-}

module Main where

import Models.Room.Room (Room(..), Direction(..), navigateToRoom, getRoom, createRoom)
import Actions (Action(..))
import Control.Monad
import Data.Maybe

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

-- Split this into pieces
handleCommands :: App -> IO ()
handleCommands app =
  case states app of
    Nothing -> do
      handleRoomUpdate app -- Send back info to the client
      newApp' <- pure $ stackState app -- Push the stack
      handleCommands newApp' -- Begin Again
    Just _ -> do
      handleRoomUpdate app -- Send back info to the client
      newApp' <- pure $ stackState app -- Push the stack
      input <- createShell -- Wait for inut
      -- Call actionParser at this point and decide between any action, not just update room
      newApp <- updateStateRoom newApp' input -- Handle input to update app
      newApp2 <- handleCreateRoom newApp input -- We begin to reduce here
      handleCommands newApp2 -- Begin Again

      -- handle UI update
        -- decide on which parts to rerender depending on what data has changed
      -- grow the state stack
      -- get input
      -- parse action
      -- maybe applyAction <$> ? (update state)
      -- loop

handleRoomUpdate :: App -> IO ()
handleRoomUpdate app =
  case states app of
    Just states' ->
      when (currentRoom /= lastRoom) (printStatus app)
      where currentRoom = room (currentState app)
            lastRoom = room $ last states'
    Nothing -> printStatus app

-- f :: Int -> Int
-- f x = if
--    | x > 5 -> x
--    | otherwise -> 0

printStatus :: App -> IO ()
printStatus app =
  let currentState' = currentState app in do
  putStrLn ""
  putStrLn $ title (room currentState')
  putStrLn $ "\x1b[32m" ++ "Exits: " ++ roomExitsToString (room currentState') ++ "\x1b[0m" ++ "\n"
  putStrLn $ description (room currentState')


updateRoom :: App -> Maybe Room -> App
updateRoom app (Just r) = app { currentState = (currentState app) { room = r } }
updateRoom app _ = app
--0
safeCreateRoomAction :: [String] -> Maybe Action
safeCreateRoomAction [name, title, description] = Just $ CreateRoomAction name title description -- you can destructure directly by value too, like in asdf
safeCreateRoomAction _ = Nothing

createAction :: [String] -> Maybe Action
createAction (verb:params) = case verb of
  "/create" -> safeCreateRoomAction params -- How to be sure we have the correct amount of strings to create the proper action?
  _ -> Nothing

applyAction :: Action -> App -> IO App -- we need something to unwrap an action and pass it as params to createRoom
applyAction (CreateRoomAction name title description) app = do
  _ <- createRoom name title description
  pure app

actionParser :: String -> Maybe Action
actionParser input = case input of
  "" -> Nothing
  _ -> createAction $ words input
  -- "n"
  -- "s"

f1 :: App -> String -> [Maybe Room] -> App
f1 app actions = \[x, y] ->
  let xr = updateRoom app x
      yr = updateRoom app y
  in case actions of
    "n" -> xr
    "s" -> yr
    _ -> app

-- Handle other situations
updateStateRoom :: App -> String -> IO App
updateStateRoom app actions =
  let r = room (currentState app)
  -- read this over and over again until you understand it
  -- [x, y] <- sequence $ fmap (navigateToRoom r) [N, S]
  in f1 app actions <$> traverse (navigateToRoom r) [N, S]

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
  handleCommands App { states = Nothing , currentState = initialState }
