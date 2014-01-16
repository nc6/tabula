{-# LANGUAGE LambdaCase #-}
{-
Agents for data collection. These are actually small programs
which are invoked from the shell and report back to the collection
daemon.
-}
module Tabula.Internal.Agent (trap, prompt) where

  import Data.Aeson (encode)
  import Data.List (intercalate)
  import Data.Time.Clock

  import Network.Socket
  import Network.Socket.ByteString.Lazy (sendAll)

  import System.Environment (getEnvironment, lookupEnv)
  import System.Posix.Directory (getWorkingDirectory)

  import qualified Tabula.Internal.Event as E

  trap :: [String] -> IO ()
  trap args = case args of
    pid : ppid : cmd' -> do
      sock <- lookupEnv "TABULA_PORT"
      case sock of
        Just sockAddr -> do 
          let cmd = unwords cmd'
          time <- getCurrentTime
          env <- getEnvironment
          let msg = E.Debug time cmd (read pid) (read ppid) env
          -- connect to socket, send the entire thing
          soc <- socket AF_UNIX Stream 0
          connect soc (SockAddrUnix sockAddr)
          sendAll soc . encode $ msg
          close soc
        Nothing -> error $ 
          "TABULA_PORT not specified. Are you sure you are running a tabula session?"
    _ -> error $ "Incorrect arguments specified:\n" ++ intercalate "\n\t" args

  prompt :: [String] -> IO ()
  prompt args = case args of
    exitCode : cmd' -> do
      sock <- lookupEnv "TABULA_PORT"
      case sock of
        Just sockAddr -> do
          let cmd = unwords cmd'
          time <- getCurrentTime
          cwd <- getWorkingDirectory
          env <- getEnvironment
          let msg = E.Prompt time env cwd cmd (read exitCode)
          -- connect to socket, send the entire thing
          soc <- socket AF_UNIX Stream 0
          connect soc (SockAddrUnix sockAddr)
          sendAll soc . encode $ msg
          close soc
        Nothing -> error $ 
          "TABULA_PORT not specified. Are you sure you are running a tabula session?"
    _ -> error $ "Incorrect arguments specified:\n" ++ intercalate "\n\t" args