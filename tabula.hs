{-# LANGUAGE LambdaCase #-}
module Main where
  import Control.Arrow
  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM (atomically)
  import Control.Concurrent.STM.TBMChan (newTBMChan)
  import Control.Monad (ap, join)

  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import Data.Conduit.TMChan (sinkTBMChan)
  import qualified Data.Map as Map
  import Data.Maybe (fromMaybe)

  import Network.Socket

  import System.Directory (removeFile)
  import System.Environment (getArgs, getExecutablePath, getEnvironment, lookupEnv)
  import System.Exit (exitWith)
  import System.IO (Handle(), hPutStrLn, stdin, stdout, stderr)
  import System.Log.Logger
  import System.Posix.IO (fdToHandle)
  import System.Posix.Terminal
  import System.Process

  import Tabula.TTY
  import Tabula.Internal.Agent
  import Tabula.Internal.Daemon

  main :: IO ()
  main = getArgs >>= \case
    "trap" : rest -> trap rest
    "prompt" : rest -> prompt rest
    _ -> updateGlobalLogger "tabula" (setLevel DEBUG) >> showShell

  showShell :: IO ()
  showShell = do
    -- Start two pseudo-terminals. We'll use one for in/err and the other for out
    (pty1m, pty1s) <- openPtyHandles
    (pty2m, pty2s) <- openPtyHandles
    --Tee off all of them
    inChan <- tee 16 stdin pty1m
    errChan <- tee 16 pty1m stderr
    outChan <- tee 16 pty2m stdout
    -- Start listening daemon in background thread
    soc <- daemon (inChan, outChan, errChan)
    debugM "tabula" $ "Setting parent terminal to raw mode."
    exitStatus <- getControllingTerminal >>= \pt -> bracketChattr pt setRaw $ do
      -- Configure PROMPT_COMMAND
      tabula <- getExecutablePath
      oldEnv <- fmap Map.fromList getEnvironment
      sn <- socketName soc
      let promptCommand = tabula ++ " prompt " ++ sn ++ " $? $(history 1)"
          newEnv = Map.toAscList $ Map.insert "PROMPT_COMMAND" promptCommand oldEnv
      -- Display a shell
      myShell <- fmap (fromMaybe "/bin/sh") $ lookupEnv "SHELL"
      (_,_,_,ph) <- createProcess $ (proc myShell ["-il"]) {
          std_in = UseHandle pty1s
        , std_out = UseHandle pty2s
        , std_err = UseHandle pty1s
        , env = Just newEnv
        , delegate_ctlc = True
      }
      -- Configure debug trap
      let trapCommand = "trap '" ++ tabula ++ " trap " ++
                        sn ++ " $BASHPID $PPID $BASH_COMMAND' DEBUG"
      hPutStrLn pty1m trapCommand
      -- Wait for exit
      waitForProcess ph
    -- Clean up the socket
    cleanSocket soc
    exitWith exitStatus
    where 
      openPtyHandles = do
        pty <- openPseudoTerminal
        getControllingTerminal >>= \a -> cloneAttr a (fst pty)
        s <- getTerminalName . snd $ pty
        debugM "tabula" $ "Acquired pseudo-terminal:\n\tSlave: " ++ s
        uncurry (ap . fmap (,)) . join (***) fdToHandle $ pty
      socketName soc = do
        name <- getSocketName soc
        case name of 
          SockAddrUnix sn -> return sn
          _ -> error "No valid socket address."
      cleanSocket soc = socketName soc >>= removeFile

  tee :: Int -> Handle -> Handle -> IO BSChan
  tee bufSize from to = do
    chan <- atomically $ newTBMChan bufSize
    _ <- forkIO . runResourceT $ DCB.sourceHandle from
          $= DCB.conduitHandle to -- Sink contents to out Handle
          $$ sinkTBMChan chan
    return chan