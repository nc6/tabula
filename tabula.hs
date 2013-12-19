module Main where
  import Control.Arrow
  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM (atomically)
  import Control.Concurrent.STM.TBMChan (TBMChan(), newTBMChan)
  import Control.Monad (ap, join)

  import Data.ByteString (ByteString)
  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)
  import Data.Maybe (fromMaybe)

  import System.Environment (lookupEnv)
  import System.Exit (exitWith)
  import System.IO (Handle(), stdin, stdout, stderr)
  import System.Posix.IO (fdToHandle)
  import System.Posix.Terminal
  import System.Process

  import Tabula.TTY

  type BSChan = TBMChan ByteString

  main :: IO ()
  main = showShell

  showShell :: IO ()
  showShell = getControllingTerminal >>= \pt -> bracketChattr pt setRaw $ do
      -- Start two pseudo-terminals. We'll use one for in/err and the other for out
      (pty1m, pty1s) <- openPtyHandles
      (pty2m, pty2s) <- openPtyHandles
      -- Tee off all of them
      inChan <- tee 256 stdin pty1m
      errChan <- tee 256 pty1m stderr
      outChan <- tee 256 pty2m stdout
      -- Start listening daemon in background thread

      -- Configure PROMPT_COMMAND
      -- Display a shell
      myShell <- fmap (fromMaybe "/bin/sh") $ lookupEnv "SHELL"
      (_,_,_,ph) <- createProcess $ (proc myShell ["-i"]) {
          std_in = UseHandle pty1s
        , std_out = UseHandle pty2s
        , std_err = UseHandle pty1s
        , delegate_ctlc = True
      }
      waitForProcess ph >>= exitWith
    where openPtyHandles = 
            openPseudoTerminal >>= uncurry (ap . fmap (,)) . join (***) fdToHandle

  daemon :: (BSChan, BSChan, BSChan) -> IO ()
  daemon (inC, outC, errC) = do
    _ <- forkIO $ runResourceT $ sourceTBMChan inC $$ DCB.sinkFile "test_in"
    _ <- forkIO $ runResourceT $ sourceTBMChan outC $$ DCB.sinkFile "test_out"
    _ <- forkIO $ runResourceT $ sourceTBMChan errC $$ DCB.sinkFile "test_err"
    return ()

  tee :: Int -> Handle -> Handle -> IO BSChan
  tee bufSize from to = do
    chan <- atomically $ newTBMChan bufSize
    _ <- forkIO . runResourceT $ DCB.sourceHandle from
          $= DCB.conduitHandle to -- Sink contents to out Handle
          $$ sinkTBMChan chan
    return chan