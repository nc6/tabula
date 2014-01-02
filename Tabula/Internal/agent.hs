{-# LANGUAGE LambdaCase #-}
{-
Agents for data collection. These are actually small programs
which are invoked from the shell and report back to the collection
daemon.
-}
module Tabula.Internal.Agent (trap, prompt) where
  --import Control.Monad (unless)

  import Data.Aeson (encode)
  import Data.List (intercalate)
  import Data.Time.Clock

  import Network.Socket
  import Network.Socket.ByteString.Lazy (sendAll)

  import System.Environment (getEnvironment)
  import System.Posix.Directory (getWorkingDirectory)

  import qualified Tabula.Internal.Event as E

  trap :: [String] -> IO ()
  trap args = case args of
    sockAddr : pid : ppid : cmd' -> do
      let cmd = intercalate " " cmd'
      time <- getCurrentTime
      env <- getEnvironment
      let msg = E.Debug time cmd (read pid) (read ppid) env
      -- connect to socket, send the entire thing
      soc <- socket AF_UNIX Stream 0
      connect soc (SockAddrUnix sockAddr)
      sendAll soc . encode $ msg
      sClose soc
    _ -> error $ "Incorrect arguments specified:\n" ++ intercalate "\n\t" args

  prompt :: [String] -> IO ()
  prompt args = case args of
    sockAddr : exitCode : cmd' -> do
      let cmd = intercalate " " cmd'
      time <- getCurrentTime
      cwd <- getWorkingDirectory
      env <- getEnvironment
      let msg = E.Prompt time env cwd cmd (read exitCode)
      -- connect to socket, send the entire thing
      soc <- socket AF_UNIX Stream 0
      connect soc (SockAddrUnix sockAddr)
      sendAll soc . encode $ msg
      sClose soc
    _ -> error $ "Incorrect arguments specified:\n" ++ intercalate "\n\t" args