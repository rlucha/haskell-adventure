{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import Web.Scotty
import Db

data User = User {
    userId :: Int
  , userName :: String
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

bob =   User { userId = 1, userName = "bob" }
jenny = User { userId = 2, userName = "jenny" }

allUsers = [bob, jenny]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

hello :: ActionM ()
hello = text "Hello world"

routes :: ScottyM ()
routes =
  -- get "/hello/:name" $ do
  --   name <- param "name"
  --   text ("Hello " <> name <> "!")

  -- get "/users" $ do json allUsers

  -- get "/users/:id" $ do
  --   id <- param "id"
  --   json (filter (matchesId id) allUsers)

  get "/test" $ do
    users <- Db.getAllUsers
    text users

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 4888 routes
