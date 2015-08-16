{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Interpreter        (runProgram)
import Compiler           (compile)
import Debugger           (debug)
import Parser             (parseProgram)

import Data.Text
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import           Control.Monad      (forever)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

meow :: WS.Connection -> IO ()
meow conn = forever $ do
    msg <- WS.receiveData conn
    WS.sendTextData conn $ msg `T.append` ", meow"

newServerState :: ServerState
newServerState = []

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state