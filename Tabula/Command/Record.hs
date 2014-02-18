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
module Tabula.Command.Record (
    record
  ) where
  import Control.Applicative (liftA)
  import Control.Concurrent (forkIO)
  import Control.Concurrent.MVar (takeMVar)
  import Control.Concurrent.STM (atomically)
  import Control.Concurrent.STM.TBMChan (newTBMChan, writeTBMChan)

  import Data.Aeson (Result(..), fromJSON)
  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import Data.Conduit.TMChan (sinkTBMChan)
  import Data.Foldable (for_)
  import qualified Data.Map as Map

  import Network.Socket

  import System.Directory (removeFile)
  import System.Environment (getExecutablePath, getEnvironment)
  import System.Exit (ExitCode, exitWith)
  import System.IO (Handle(), hPutStrLn, stdin, stdout, stderr)

  import Tabula.Destination
  import Tabula.Record (entry)
  import Tabula.Record.Console (priorEnv, posteriorEnv, workingDirectory)
  import Tabula.Record.Environment
  import Tabula.Internal.Daemon
  import Tabula.Shell
  import Tabula.TTY

  record :: Destination
          -> Bool
          -> Int
          -> IO ()
  record dest resume bufSize = do
      oldRecord <- if resume then 
          liftA (>>= fromJSONMaybe . entry) $ getLastRecord dest 
        else 
          return Nothing
      -- Set the old environment
      oldEnv <- case oldRecord of
        Just e ->
          return . Map.fromList $ indifferentMerge (posteriorEnv e) (priorEnv e)
        Nothing -> getEnvironment >>= return . addHistIgnore . Map.fromList
          where addHistIgnore env = (flip $ Map.insert "HISTIGNORE") env $
                  "[ \\t]*" ++ case Map.lookup "HISTIGNORE" env of
                    Just a -> ":" ++ a
                    Nothing -> "" 

      -- Create a shell
      shell@(Shell i _ _ _) <- create (Just . Map.toAscList $ oldEnv)
      -- Set controlling terminal to raw:
      getControllingTerminal >>= \pt -> bracketChattr pt setRaw $ do
          -- Change directory
          for_ oldRecord $ hPutStrLn i . ("cd " ++) . workingDirectory
          startRecording shell dest bufSize 
        >>= exitWith
    where
      fromJSONMaybe a = case fromJSON a of
        Error _ -> Nothing
        Success b -> Just b
  
  startRecording :: ProcessWait a => 
             Shell a -- ^ Shell to record.
          -> Destination -- ^ Destination to record to.
          -> Int -- ^ Buffer size
          -> IO ExitCode
  startRecording s@(Shell i o e ps) dest bufSize = do
    -- Fetch basic information
    tabula <- getExecutablePath

    -- Tee off all of them
    inChan <- tee bufSize stdin i
    errChan <- tee bufSize e stderr
    outChan <- tee bufSize o stdout
    stopChan <- atomically $ newTBMChan 1 -- Just contains the 'Stop' message
    let channels = (inChan, errChan, outChan, stopChan)
        promptCommand = tabula ++ " prompt \\$? \\$(history 1 | tr -s ' ' | cut -d' ' -f3-)"
        promptCommandUnquoted = tabula ++ " prompt $? $(history 1 | tr -s ' ' | cut -d' ' -f3-)"
        trapCommand = "trap '" ++ tabula ++ " trap $BASHPID $PPID $BASH_COMMAND' DEBUG"

    injectEnv s "PROMPT_COMMAND" promptCommand
    putIgnoredLn i $ trapCommand
    -- Start listening daemon in background thread
    (done, soc) <- daemon dest channels 
                    [promptCommand, trapCommand, promptCommandUnquoted, "clear"] 
                    bufSize
    sn <- socketName soc
    -- Inject relevant things
    injectEnv s "TABULA_PORT" sn
    putIgnoredLn i "clear"
    -- Display the shell to the user
    exitStatus <- waitRemote ps
    socFile <- socketName soc
    -- Close the pipes
    atomically $ writeTBMChan stopChan ()
    -- Wait for daemon thread to exit
    takeMVar done
    -- Clean up the socket
    removeFile socFile
    return exitStatus
    where 
      socketName soc = do
        name <- getSocketName soc
        case name of 
          SockAddrUnix sn -> return sn
          _ -> error "No valid socket address."
      putIgnoredLn h = hPutStrLn h . (" " ++)

  tee :: Int -> Handle -> Handle -> IO BSChan
  tee bufSize from to = do
    chan <- atomically $ newTBMChan bufSize
    _ <- forkIO . runResourceT $ DCB.sourceHandle from
          $= DCB.conduitHandle to -- Sink contents to out Handle
          $$ sinkTBMChan chan True
    return chan