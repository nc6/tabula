module Tabula.Exec (exec) where
  import Prelude hiding (sequence)

  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM (atomically)
  import Control.Concurrent.STM.TBMChan (newTBMChan)
  import Control.Exception.Base (bracket)
  
  import qualified Data.ByteString.Lazy.Char8 as B
  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)
  import Data.Time.Clock
  import Data.Traversable (sequence)

  import Network.BSD (getHostName)

  import System.Environment (getEnvironment)
  import System.Exit
  import System.IO
  import System.Posix.Directory (getWorkingDirectory)
  import System.Process
  import System.Posix.IO (fdToHandle, openFd, OpenMode(ReadWrite), defaultFileFlags)
  import System.Posix.Terminal 
  import System.Posix.Types (Fd(..))

  import qualified Tabula.Record as Rec
  import qualified Tabula.Record.Console as Rec.Cons
  import Tabula.Destination (Destination(), makeEntry)

  exec :: Destination -> String -> IO ()
  exec dest command = withRawPty $ \(ptym,ptys) -> do
    hostName <- getHostName
    workingDirectory <- getWorkingDirectory
    priorEnv <- getEnvironment
    startTime <- getCurrentTime
    (_, _, hErr, ph) <- createProcess $ (shell command) { 
          delegate_ctlc = True
        , std_err = CreatePipe
        , std_out = UseHandle ptys
        , std_in = UseHandle ptys
      }
    snipOut <- tee 256 ptym stdout
    snipErr <- sequence $ fmap (\h -> tee 256 h stderr) hErr
    snipIn <- tee 256 stdin ptym
    exitCode <- waitForProcess ph
    endTime <- getCurrentTime
    posteriorEnv <- getEnvironment
    let record = Rec.record (Rec.Cons.ConsoleRecord 
                    command
                    hostName
                    workingDirectory
                    priorEnv
                    posteriorEnv
                    startTime
                    endTime
                    (Just snipIn)
                    (Just snipOut)
                    snipErr
                    (exitCodeInt exitCode)
                )
    makeEntry dest record

  tee :: Int -> Handle -> Handle -> IO B.ByteString
  tee bytes from to = do
    chan <- atomically $ newTBMChan bytes
    _ <- forkIO . runResourceT $ DCB.sourceHandle from
          $= DCB.conduitHandle to -- Sink contents to out Handle
          $$ sinkTBMChan chan
    runResourceT $ sourceTBMChan chan $$ DCB.take bytes -- Pull off the start of the stream

  exitCodeInt :: ExitCode -> Int
  exitCodeInt ExitSuccess = 0
  exitCodeInt (ExitFailure a) = a

  withRawPty :: ((Handle, Handle) -> IO a) -> IO a
  withRawPty action = do
      cTerm <- getControllingTerminalName >>= 
            \a -> openFd a ReadWrite Nothing defaultFileFlags
      parentAttrs <- getTerminalAttributes cTerm
      bracket (initialise parentAttrs cTerm)
              (finalise parentAttrs cTerm)
              action
    where
      initialise :: TerminalAttributes -> Fd -> IO (Handle, Handle)
      initialise parentAttrs cTerm = do
        (ptym, ptys) <- openPseudoTerminal
        setTerminalAttributes ptym parentAttrs Immediately
        setTerminalAttributes cTerm (withoutModes rawModes parentAttrs) Immediately
        hm <- fdToHandle ptym
        hs <- fdToHandle ptys
        return (hm, hs)

      finalise :: TerminalAttributes -> Fd -> (Handle, Handle) -> IO ()
      finalise parentAttrs cTerm (hm, hs) = do
        hClose hm
        hClose hs
        setTerminalAttributes cTerm parentAttrs Immediately

      rawModes = [ProcessInput, KeyboardInterrupts, ExtendedFunctions, 
                  EnableEcho, InterruptOnBreak, MapCRtoLF, IgnoreBreak, 
                  IgnoreCR, MapLFtoCR, CheckParity, StripHighBit, 
                  StartStopOutput, MarkParityErrors, ProcessOutput]
      withoutModes modes tty = foldl withoutMode tty modes