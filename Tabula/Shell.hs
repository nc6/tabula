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
module Tabula.Shell (
    create
  , injectEnv
  , Shell(Shell)
) where
  import Control.Arrow
  import Control.Monad (ap, join)

  import Data.Maybe (fromMaybe)

  import System.Environment (lookupEnv)
  import System.IO (Handle(), hPutStrLn)
  import System.Log.Logger
  import System.Posix.IO (fdToHandle)
  import System.Posix.Terminal
  import System.Process

  import Tabula.Record.Environment
  import Tabula.TTY

  data Shell = Shell Handle Handle Handle ProcessHandle

  create :: Maybe Env -> IO Shell
  create newEnv = do
    (pty1m, pty1s) <- openPtyHandles
    (pty2m, pty2s) <- openPtyHandles
    ph <- do
      myShell <- fmap (fromMaybe "/bin/sh") $ lookupEnv "SHELL"
      (_,_,_,ph) <- createProcess $ (proc myShell ["-il"]) {
          std_in = UseHandle pty1s
        , std_out = UseHandle pty2s
        , std_err = UseHandle pty1s
        , env = newEnv
        , delegate_ctlc = True
      }
      return ph
    return $ Shell pty1m pty2m pty1m ph
    where 
      openPtyHandles = do
        pty <- openPseudoTerminal
        getControllingTerminal >>= \a -> cloneAttr a (fst pty)
        s <- getTerminalName . snd $ pty
        debugM "tabula" $ "Acquired pseudo-terminal:\n\tSlave: " ++ s
        uncurry (ap . fmap (,)) . join (***) fdToHandle $ pty

  -- | Inject an environment variable into the running process.
  injectEnv :: Shell -> String -> String -> IO ()
  injectEnv (Shell i _ _ _) key value = let
      cmd = "export " ++ key ++ "=\"" ++ value ++ "\""
    in hPutStrLn i cmd