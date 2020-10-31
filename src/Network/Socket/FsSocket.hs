-- | 

module Network.Socket.FsSocket (pathFromString, runTcpServer, runTcpClient, FsSocketPath(..)) where

import qualified Network.Socket as Sock
import Network.Socket (SockAddr(..), SocketType, AddrInfo, Socket)
import Data.Foldable (foldl')
import qualified Data.Char as Char
import Data.Function ((&))
import Control.Concurrent (forkFinally)
import Control.Monad (forever, void)
import qualified Control.Exception as E

newtype FsSocketPath = FsSocketPath { getSockPath :: String }
  deriving (Eq, Show)

maxSocketPathLen = 104

pathFromString :: String -> Either String FsSocketPath
pathFromString str
  | not allCodePointsLT256 =
      Left $ "Not all code points in '" ++ str ++ "' are less than 256'"
  | length str >= maxSocketPathLen =
      Left $ "Socket path name must be less than " ++ show maxSocketPathLen ++ " characters."
  | otherwise = Right $ FsSocketPath str
  where allCodePointsLT256 =
          map Char.ord str
          & map (<256)
          & foldl' (&&) True

-- Goal: Path -> stream socket
-- Stream - Sock.Stream

-- openSocket :: AddrInfo -> IO Socket
-- socket :: Family -> SocketType -> ProtocolNomuber -> IO SOCKET
sockForPath :: IO Socket
sockForPath = Sock.socket Sock.AF_UNIX Sock.Stream Sock.defaultProtocol

bind :: Socket -> FsSocketPath -> IO ()
bind sock (FsSocketPath path) = Sock.bind sock $ Sock.SockAddrUnix path

connect :: Socket -> FsSocketPath -> IO ()
connect sock (FsSocketPath path) = Sock.connect sock $ Sock.SockAddrUnix path

runTcpServer :: FsSocketPath -> Int -> (Socket -> IO a) -> IO ()
runTcpServer spath finTimeout server =
  E.bracketOnError sockForPath Sock.close (\sock -> do
        bind sock spath
        Sock.listen sock Sock.maxListenQueue
        loop sock
        Sock.close sock
        return ()
    )
  where
    loop sock = forever $ E.bracketOnError (Sock.accept sock) (Sock.close . fst)
      $ \(connSock, _peerAddr) -> void $
          forkFinally
            (server connSock)
            (const $ Sock.gracefulClose connSock finTimeout)

runTcpClient :: FsSocketPath -> (Socket -> IO a) -> IO ()
runTcpClient spath client =
  E.bracketOnError sockForPath Sock.close (\sock -> do
        connect sock spath
        client sock
        Sock.close sock
        return ()
    )
