-- | 

module Beelzebub.Daemon (createDaemon) where

import qualified System.IO as IO
import System.Posix.Process (createSession)
import System.Posix.Files (setFileCreationMask)
import System.Directory (setCurrentDirectory)
import System.Posix.Types (CMode(..))

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
