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