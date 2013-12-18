module Main where
  import Prelude hiding (sequence)

  import Data.List (isPrefixOf)
  import Data.Time.Clock
  import Data.Traversable (sequence)

  import Network.BSD (getHostName)
  import Network.URL

  import qualified System.Console.Readline as RL
  import System.Environment (getEnvironment)
  import System.Exit
  import System.IO
  import System.Posix.Directory (getWorkingDirectory)
  import System.Process
  import System.Posix.IO (fdToHandle, openFd, OpenMode(ReadWrite), defaultFileFlags)
  import System.Posix.Terminal 

  import Tabula.Exec
  import qualified Tabula.Record as Rec
  import qualified Tabula.Record.Console as Rec.Cons
  import Tabula.Destination (makeEntry)
  import qualified Tabula.Destination.File as FD
  import qualified Tabula.Strings as S

  data Destination = File FilePath 
                   | Port Int
                   | HTTP URL

  data Options = Options {
    optDestination :: Destination
  }

  main :: IO ()
  main = readEvalPrintLoop

  readEvalPrintLoop :: IO ()
  readEvalPrintLoop = do
      pty <- do
        parentTerminal <- getControllingTerminalName >>= 
                          \a -> openFd a ReadWrite Nothing defaultFileFlags
        sttyp <- getTerminalAttributes parentTerminal
        (a, b) <- openPseudoTerminal
        let rawModes = [ProcessInput, KeyboardInterrupts, ExtendedFunctions, 
                        EnableEcho, InterruptOnBreak, MapCRtoLF, IgnoreBreak, 
                        IgnoreCR, MapLFtoCR, CheckParity, StripHighBit, 
                        StartStopOutput, MarkParityErrors, ProcessOutput]
            sttym = withoutModes rawModes sttyp
            withoutModes modes tty = foldl withoutMode tty modes
        setTerminalAttributes b sttym Immediately
        setTerminalAttributes a sttym Immediately
        a' <- fdToHandle a
        b' <- fdToHandle b
        return (a',b')

      maybeLine <- RL.readline "% "
      case maybeLine of
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just mc | isPrefixOf ":" mc -> do
          evalMetaCommand (drop 1 mc)
          readEvalPrintLoop
        Just line -> do 
          RL.addHistory line
          eval pty line
          readEvalPrintLoop

  eval :: (Handle, Handle) -> String -> IO ()
  eval pty command = do
      let (ptym, ptys) = pty
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
      makeEntry fileDest record
    where fileDest = FD.fileDestination "typescript"

  evalMetaCommand :: String -> IO ()
  evalMetaCommand command = case command of
    a -> putStrLn $ S.metaCommandNotFound a