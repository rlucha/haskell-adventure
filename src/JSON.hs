{-#  LANGUAGE OverloadedStrings #-}
{-#  LANGUAGE DeriveGeneric #-}

module JSON where

import GHC.Generics

import Data.Aeson

data Player = Player
  { playerName :: String
  , playerAge :: Int
  } deriving (Show, Generic)

instance ToJSON Player
instance FromJSON Player

id :: a -> String
id a = show a


data X = X deriving (Show)

main = do
  print X

  print $ (decode (encode (Player "Roberto"  28)) :: Maybe Player)
