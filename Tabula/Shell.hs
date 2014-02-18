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
{-# LANGUAGE LambdaCase, TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
module Tabula.Shell (
    create
  , injectEnv
  , Shell(Shell)
  , ProcessWait
  , waitRemote
) where
  import Control.Arrow
  import Control.Monad (ap, join)

  import Data.Maybe (fromMaybe)

  import System.Environment (lookupEnv)
  import System.Exit
  import System.IO (Handle(), hPutStrLn)
  import System.Log.Logger
  import System.Posix.IO
  import System.Posix.Process
#ifdef OS_LINUX
  import System.Posix.Signals
  import System.Posix.Signals.Exts
#endif
  import System.Posix.Terminal
  import System.Posix.Types (Fd, ProcessID)
  import System.Process

  import Tabula.Record.Environment
  import Tabula.TTY

  class ProcessWait a where
    waitRemote :: a -> IO ExitCode

  instance ProcessWait ProcessHandle where
    waitRemote = waitForProcess

  instance ProcessWait ProcessID where
    waitRemote pid = getProcessStatus True False pid >>= \case
      Just (Exited exitCode) -> return exitCode
      _ -> return $ ExitFailure 1

  data Shell a = Shell Handle Handle Handle a

  create :: Maybe Env -> IO (Shell ProcessID)
  create newEnv = do
    (pty1m, pty1s) <- openPtyHandles
    (pty2m, pty2s) <- openPtyHandles
    ph <- forkProcess $ shellProcess pty1s pty2s pty1s
    (h1m, h2m) <- uncurry (ap . fmap (,)) . join (***) fdToHandle $ (pty1m, pty2m)
    return $ Shell h1m h2m h1m ph
    where 
      openPtyHandles = do
        pty <- openPseudoTerminal
#ifdef OS_LINUX
        installHandler sigWINCH (Catch . setWindowSize . fst $ pty) Nothing
        setWindowSize (fst pty)
#endif
        getControllingTerminal >>= \a -> cloneAttr a (fst pty)
        s <- getTerminalName . snd $ pty
        debugM "tabula" $ "Acquired pseudo-terminal:\n\tSlave: " ++ s
        return pty
      shellProcess :: Fd -> Fd -> Fd -> IO ()
      shellProcess i o e = do
        myShell <- fmap (fromMaybe "/bin/sh") $ lookupEnv "SHELL"
        dupTo i stdInput
        dupTo o stdOutput
        dupTo e stdError
        mapM_ closeFd [i,o]
        createSession
        setControllingTerminal stdInput
        executeFile myShell False ["-il"] newEnv
        exitImmediately ExitSuccess

  -- | Inject an environment variable into the running process.
  injectEnv :: (ProcessWait a) => Shell a -> String -> String -> IO ()
  injectEnv (Shell i _ _ _) key value = let
      cmd = " export " ++ key ++ "=\"" ++ value ++ "\""
    in hPutStrLn i cmd
