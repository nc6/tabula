{-# LANGUAGE LambdaCase #-}
{-
Agents for data collection. These are actually small programs
which are invoked from the shell and report back to the collection
daemon.
-}
module Tabula.Internal.Agent (trap, prompt) where
  import Data.Aeson (encode)
  import Data.Time.Clock

  import Network.Socket
  import Network.Socket.ByteString.Lazy (sendAll)

  import System.Environment (getEnv, getEnvironment)
  import System.Posix.Directory (getWorkingDirectory)

  import qualified Tabula.Internal.Event as E

  trap :: [String] -> IO ()
  trap args = case args of
    sockAddr : [] -> do
      time <- getCurrentTime
      cmd <- getEnv "BASH_COMMAND"
      pid <- fmap read $ getEnv "BASHPID"
      ppid <- fmap read $ getEnv "PPID"
      env <- getEnvironment
      let msg = E.Debug time cmd pid ppid env
      -- connect to socket, send the entire thing
      soc <- socket AF_UNIX Stream 0
      connect soc (SockAddrUnix sockAddr)
      sendAll soc . encode $ msg
      sClose soc
    _ -> error "No socket address supplied."

  prompt :: [String] -> IO ()
  prompt args = case args of
    sockAddr : cmd : exitCode : [] -> do
      time <- getCurrentTime
      cwd <- getWorkingDirectory
      env <- getEnvironment
      let msg = E.Prompt time env cwd cmd (read exitCode)
      -- connect to socket, send the entire thing
      soc <- socket AF_UNIX Stream 0
      connect soc (SockAddrUnix sockAddr)
      sendAll soc . encode $ msg
      sClose soc
    _ -> error "No socket address supplied."