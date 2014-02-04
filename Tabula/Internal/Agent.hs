{-
Copyright (c) 2014 Genome Research Ltd.

Author: Nicholas A. Clarke <nicholas.clarke@sanger.ac.uk>

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.
-}
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
          sendMsg sockAddr msg
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
          sendMsg sockAddr msg
        Nothing -> error $ 
          "TABULA_PORT not specified. Are you sure you are running a tabula session?"
    _ -> error $ "Incorrect arguments specified:\n" ++ intercalate "\n\t" args

  sendMsg :: FilePath -> E.Event -> IO ()
  sendMsg sockAddr msg = do
    soc <- socket AF_UNIX Stream 0
    connect soc (SockAddrUnix sockAddr)
    sendAll soc . encode $ msg
    close soc