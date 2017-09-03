{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class

import Db

getUserHandler :: ActionM()
getUserHandler = do
  id <- param "id"
  user <- liftIO (Db.getUser id) -- liftIO to actionM
  json user

getAllUsersHandler :: ActionM()
getAllUsersHandler = do
  users <- liftIO Db.getAllUsers -- liftIO to actionM
  json users

routes :: ScottyM ()
routes = do
  get "/users" getAllUsersHandler
  get "/users/:id" getUserHandler

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 4888 routes
