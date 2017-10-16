module Actions where

type RoomName = String
type RoomTitle = String
type RoomDescription = String

data Action = CreateRoomAction RoomName RoomTitle RoomDescription
            | CreateExitAction String String
