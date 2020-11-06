{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process.Typed as P
import System.Posix.Process as PosixProcess
import System.Posix.Types (CPid(..), CMode(..))
import Control.Concurrent (threadDelay)
import System.Posix.Files (setFileCreationMask)
import System.Directory (setCurrentDirectory)
import System.IO as IO
import qualified Beelzebub.Daemon as Daemon
import qualified Beelzebub.FsSocket as FsSocket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, sendAll)

import Lib

main :: IO ()
main =
  case FsSocket.pathFromString "/home/pen/test_sock" of
    Left err -> putStrLn err
    Right path -> do
      FsSocket.runTcpServer path 5000 server
  where
    server sock = do
      sendAll sock "Hello, world!"
      msg <- recv sock 1024
      putStr "Recieved: "
      C.putStrLn msg

--main :: IO ()
--main = do
--  pid <- PosixProcess.forkProcess child
--  parent pid

child :: IO ()
child = do
  putStrLn $ "I am the child"
  oldPGID <- getProcessGroupID
  newPGID <- createSession
  putStrLn $ "My old processess group id was: " <> show oldPGID <> " my new one is " <> show newPGID
  threadDelay 200000000
  
parent :: CPid -> IO ()
parent pid = do
  pgid <- getProcessGroupID
  putStrLn $ "I am the parent with child pid: " <> show pid <> " and process group id: " <> show pgid
  threadDelay 200000000

runLs :: IO ()
runLs = do
  P.runProcess "ls" >>= print

-- Socket notes
--  Network.Socket.maxListenQueue :: Int - the value of SOMAXCONN
  

  
  
  
