{-# LANGUAGE DeriveGeneric #-}
-- |

module Beelzebub.Imp where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Control.Concurrent as Concurrent
import Control.Concurrent.STM.TVar (TVar)
import Data.IntMap.Strict (IntMap)
import Data.Time (UTCTime)
import qualified System.Posix.Process as Process
import System.Posix.Types (ProcessID)
import qualified Data.IntMap.Strict as IntMap

import Beelzebub.Imp.ExitCode (ExitCode)
import qualified Beelzebub.Imp.ExitCode as ExitCode
import GHC.Conc.Signal (Signal)
import System.IO.Error (isDoesNotExistError, IOError)
import Control.Exception (try, throw, Exception)

-- | Imp - worker child daemons
data Imp = Imp
  { status :: ImpStatus
  , command :: ()
  , startTime :: UTCTime
  }
  
data ImpStatus
  = Bedeviling
    { process :: ()
    , stdin :: ()
    , stdout :: ()
    , stderr :: ()
    , pid :: ProcessID
    }
  | Scheming ProcessID
  | OnBreak -- SIGSTP or SIGSTOP
    { process :: ()
    , stdin :: ()
    , stdout :: ()
    , stderr :: ()
    , pid :: ProcessID
    , breakStart :: UTCTime
    }
  | Foiled
    { stopTime :: UTCTime
    , why :: Signal
    , coreDumped :: Bool
    } -- failure to execv or similar
  | Banished -- Process terminated and de-zombified
    { stopTime :: UTCTime
    , exitCode :: ExitCode
    }

-- SEE: https://hackage.haskell.org/package/process-1.6.10.0/docs/src/System.Process.Posix.html#createProcess_Internal
-- also : https://github.com/haskell/process/blob/master/cbits/posix/runProcess.c
-- spawnImp = do
-- create signal
-- fork child with signal, that signals back pid before execv
-- Transition to scheming with pid
-- Signal ready for 

-- | Reference to an Imp instance
-- Can be sent over the wire to clients
newtype ImpReference = ImpReference Int
  deriving (Eq, Show, Generic)

instance FromJSON ImpReference
instance ToJSON ImpReference

-- This will probably require some context monad at some point
dereference :: Subordinants -> ImpReference -> Maybe Imp
dereference pool (ImpReference i) =
  (IntMap.lookup i . rabble) pool

data Subordinants = Subordinants
  { nextId :: Int
  , rabble :: IntMap Imp -- TODO: channel to notify watchers to faff off, wrap in concurrency primatives
  }

initialImpPool :: Subordinants
initialImpPool = Subordinants { nextId = 0, rabble = IntMap.empty }


{-
waitPid event loop:
wait for child process death
update state
-}

impWatcher :: Subordinants -> IO ()
impWatcher = do
  undefined
  

wait :: IO (ProcessID, Process.ProcessStatus)
wait =
-- wait man page for osx:
-- https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/waitpid.2.html
  go []
  where
    maxExceptions = 5
    retryDelay = 100000 -- microseconds
    go unhandledExceptions =
      let
        next = go []
        -- drop execptions on the floor until we get $maxExceptions in a row
        retry ex =
          if length unhandledExceptions < maxExceptions then
            go (ex : unhandledExceptions)
          else
            (pure . throw . WaitException . reverse) $ ex : unhandledExceptions
      in do
        -- waitpid(0, &status_loc, WUNTRACED) under the hood
        result <- try $ Process.getAnyProcessStatus True True
        case result of
          Right mTup -> case mTup of
            Just (pid, procStatus) -> pure (pid, procStatus)
            Nothing -> next -- I think this case can only happen with /really/ 
                            -- old versions of linux (<2.6.10) but I could
                            -- be wrong 😬
                            -- TODO: add logging for this
          Left ex ->
            if isDoesNotExistError ex
            then (Concurrent.threadDelay retryDelay) >> next
            else retry ex
          
newtype WaitException = WaitException [IOError]
  deriving (Show)

instance Exception WaitException
