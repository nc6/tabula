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

  import Network.Socket (Family(AF_UNIX), SocketType(Stream) 
                         , SockAddr(SockAddrUnix), Socket()
                         , bind, socket, connect)

  import System.Environment (lookupEnv)
  import System.Exit (ExitCode(), exitWith)
  import System.IO (Handle(), stdin, stdout, stderr)
  import System.Log.Logger
  import System.Posix.IO (fdToHandle)
  import System.Posix.Terminal
  import System.Process
  import System.Random (randomRIO)

  import Tabula.TTY

  type BSChan = TBMChan ByteString

  main :: IO ()
  main = updateGlobalLogger "tabula" (setLevel DEBUG) >> showShell

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
    _ <- daemon (inChan, outChan, errChan)
    debugM "tabula" $ "Setting parent terminal to raw mode."
    exitStatus <- getControllingTerminal >>= \pt -> bracketChattr pt setRaw $ do
      -- Configure PROMPT_COMMAND
      -- Display a shell
      myShell <- fmap (fromMaybe "/bin/sh") $ lookupEnv "SHELL"
      (_,_,_,ph) <- createProcess $ (proc myShell ["-il"]) {
          std_in = UseHandle pty1s
        , std_out = UseHandle pty2s
        , std_err = UseHandle pty1s
        , delegate_ctlc = True
      }
      waitForProcess ph
    exitWith exitStatus
    where 
      openPtyHandles = do
        pty <- openPseudoTerminal
        getControllingTerminal >>= \a -> cloneAttr a (fst pty)
        s <- getTerminalName . snd $ pty
        debugM "tabula" $ "Acquired pseudo-terminal:\n\tSlave: " ++ s
        uncurry (ap . fmap (,)) . join (***) fdToHandle $ pty

  daemon :: (BSChan, BSChan, BSChan) -> IO Socket
  daemon (inC, outC, errC) = do
    sockAddr <- fmap (\a -> "/tmp/tabula" ++ (show a) ++ ".soc")
      (randomRIO (0, 9999999) :: IO Int)
    -- Start up a domain socket to do the listening
    soc <- socket AF_UNIX Stream 0
    bind soc (SockAddrUnix sockAddr)
    _ <- forkIO $ runResourceT $ sourceTBMChan inC $$ DCB.sinkFile "test_in"
    _ <- forkIO $ runResourceT $ sourceTBMChan outC $$ DCB.sinkFile "test_out"
    _ <- forkIO $ runResourceT $ sourceTBMChan errC $$ DCB.sinkFile "test_err"
    return soc

  tee :: Int -> Handle -> Handle -> IO BSChan
  tee bufSize from to = do
    chan <- atomically $ newTBMChan bufSize
    _ <- forkIO . runResourceT $ DCB.sourceHandle from
          $= DCB.conduitHandle to -- Sink contents to out Handle
          $$ sinkTBMChan chan
    return chan