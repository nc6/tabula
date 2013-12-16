module Main where
  import Prelude hiding (sequence)

  import qualified Data.ByteString.Lazy.Char8 as B
  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import Data.Time.Clock
  import Data.Traversable (sequence)

  import Network.BSD (getHostName)
  import Network.URL

  import qualified System.Console.Readline as RL
  import System.Environment (getEnvironment)
  import System.Exit
  import System.IO
  import System.Process

  import qualified Tabula.Record as Rec
  import qualified Tabula.Record.Console as Rec.Cons
  import Tabula.Destination (makeEntry)
  import qualified Tabula.Destination.File as FD

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
      maybeLine <- RL.readline "% "
      case maybeLine of
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just line -> do 
          RL.addHistory line
          record <- eval line
          makeEntry fileDest record
          readEvalPrintLoop
    where fileDest = FD.fileDestination "typescript"

  eval :: String -> IO Rec.Record
  eval command = do
    hostName <- getHostName
    priorEnv <- getEnvironment
    startTime <- getCurrentTime
    (_, hOut, hErr, ph) <- createProcess $ (shell command) { 
          delegate_ctlc = True
        , std_out = CreatePipe
        , std_in = CreatePipe
      }
    snipOut <- sequence $ fmap (\h -> tee h stdout) hOut
    snipErr <- sequence $ fmap (\h -> tee h stderr) hErr
    exitCode <- waitForProcess ph
    endTime <- getCurrentTime
    posteriorEnv <- getEnvironment
    let record = Rec.record (Rec.Cons.ConsoleRecord 
                    command
                    hostName
                    priorEnv
                    posteriorEnv
                    startTime
                    endTime
                    snipOut
                    snipErr
                    (exitCodeInt exitCode)
                )
    return record

  tee :: Handle -> Handle -> IO B.ByteString
  tee from to = DCB.sourceHandle from
                $= DCB.conduitHandle to -- Sink contents to out Handle
                $$ DCB.take 256 -- Pull off the start of the stream

  exitCodeInt :: ExitCode -> Int
  exitCodeInt ExitSuccess = 0
  exitCodeInt (ExitFailure a) = a 