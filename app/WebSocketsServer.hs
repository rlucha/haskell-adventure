{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad      (forever)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS


main :: IO ()
main = do
   state <- newMVar newServerState
   WS.runServer "127.0.0.1" 9160 $ application state
