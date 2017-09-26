{-# LANGUAGE DeriveGeneric #-}

module Models.Player.Player where

import Models.Room.Room (Room)

data Player = Player {
  currentRoom :: Room
}
