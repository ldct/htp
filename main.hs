{-# LANGUAGE OverloadedStrings #-}

import Interpreter        (initialEnv)
import Debugger           (ProgramState, stepForward, stepBackward, replaceLine)
import Parser             (parseProgram, runParser, commandParser, resolveError)

import Data.Text (Text, pack, unpack)
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar, newEmptyMVar,  putMVar, tryTakeMVar, readMVar)

import           Control.Monad      (forever)
import qualified Network.WebSockets as WS

type States = [ProgramState]

application :: MVar States -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  forever $ do
    msg <- (WS.receiveData conn :: IO Text)
    maybeProgStates <- tryTakeMVar state
    case maybeProgStates of
      Nothing -> do
        putMVar state [(parseProgram . unpack $ msg, [], initialEnv, [], [])]
      Just progStates -> do
        let action = words . unpack $ msg
        case action of
          ["f"] -> putMVar state (stepForward progStates)
          ["b"] -> putMVar state (stepBackward progStates)
          ["r", num, line] -> do -- r closes the connection; fix?
            let command = (resolveError . runParser commandParser) line
            let offset = read num
            putMVar state (map (uncurry $ replaceLine command) (zip (map (+ offset) [0..]) progStates))
          _ -> putMVar state progStates
        newStates <- readMVar state
        let (restProgram, executed, env, stdout, stdin):_ = newStates
        (WS.sendTextData conn) . pack $ "stdin\n" ++ (unlines stdin)
        (WS.sendTextData conn) . pack $ "stdout\n" ++ (unlines . reverse $ stdout)
        (WS.sendTextData conn) . pack $ "linesRemaining\n" ++ (show . length $ restProgram)
        (WS.sendTextData conn) . pack $ "prog\n" ++ (unlines . (map show) $ (reverse executed) ++ restProgram)
    return ()

main :: IO ()
main = do
  state <- newEmptyMVar
  WS.runServer "0.0.0.0" 9160 $ application state
