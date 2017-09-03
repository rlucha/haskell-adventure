{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class

import Db

routes :: ScottyM ()
routes =
  get "/all_users" $ do
    user <- liftIO Db.getAllUsers -- liftIO to actionM
    json user

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 4888 routes
