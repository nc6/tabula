module Tabula.Exec where
  import Prelude hiding (sequence)

  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM (atomically)
  import Control.Concurrent.STM.TBMChan (newTBMChan)
  
  import qualified Data.ByteString.Lazy.Char8 as B
  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)
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

  import qualified Tabula.Record as Rec
  import qualified Tabula.Record.Console as Rec.Cons
  import Tabula.Destination (makeEntry)
  import qualified Tabula.Destination.File as FD
  import qualified Tabula.Strings as S

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