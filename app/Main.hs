{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process.Typed as P
import System.Posix.Process as PosixProcess
import System.Posix.Types (CPid(..), CMode(..))
import Control.Concurrent (threadDelay)
import System.Posix.Files (setFileCreationMask)
import System.Directory (setCurrentDirectory)
import System.IO as IO
import qualified Network.Socket.FsSocket as FsSocket
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

-- In general, call System.Posix.Process.forkProcess createDaemon
createDaemon :: IO () -> IO ()
createDaemon program = do
  -- Make sure we can read and write to accessed files
  setFileCreationMask $ CMode 0
  -- Set CWD to root, which is guaranteed to exist
  setCurrentDirectory "/"
  -- Make sure we outlive the parent process
  _pgid <- createSession
  -- Close standard file descriptors, as we don't need them and they're a bit of
  -- a security hazard (?)
  traverse IO.hClose [IO.stdin, IO.stdout, IO.stderr]
  -- Run the given program
  program

-- Socket notes
--  Network.Socket.maxListenQueue :: Int - the value of SOMAXCONN
  

  
  
  
