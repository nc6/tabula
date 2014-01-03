module Tabula.Internal.Daemon (daemon, BSChan) where

  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM (atomically)
  import Control.Concurrent.STM.TBMChan (newTBMChan, TBMChan())
  import Control.Monad (void)
  import Control.Monad.IO.Class

  import Data.Aeson (encode, json, fromJSON, Result(..))
  import Data.ByteString (ByteString)
  import qualified Data.ByteString.Lazy as L
  import Data.Conduit
  import Data.Conduit.Attoparsec (conduitParserEither)
  import qualified Data.Conduit.Binary as DCB
  import qualified Data.Conduit.List as DCL
  import Data.Conduit.Network
  import Data.Conduit.TMChan (sinkTBMChan, sourceTBMChan, mergeSources)

  import Network.Socket

  import System.Random (randomRIO)

  import qualified Tabula.Internal.Event as E
  import Tabula.Record (record)
  import qualified Tabula.Record.Console as Rec

  type EChan = TBMChan E.Event
  type BSChan = TBMChan ByteString

  daemon :: (BSChan, BSChan, BSChan) -> IO Socket
  daemon (inC, outC, errC) = do
    sockAddr <- fmap (\a -> "/tmp/tabula-" ++ (show a) ++ ".soc")
      (randomRIO (0, 9999999) :: IO Int)
    -- Start up a domain socket to do the listening
    soc <- socket AF_UNIX Stream 0
    bind soc (SockAddrUnix sockAddr)
    socChan <- listenSocket soc 16
    -- Merge all channels together
    let inS = sourceTBMChan inC $= DCL.map E.Stdin
        outS = sourceTBMChan outC $= DCL.map E.Stdout
        errS = sourceTBMChan errC $= DCL.map E.Stderr
        socS = sourceTBMChan socChan
    mergedS <- runResourceT $ mergeSources [inS, outS, errS, socS] 16
    -- Sink to a session list
    _ <- forkIO . runResourceT $ mergedS $= 
      conduitSession $= 
      DCL.map (encode . record) $=
      DCL.concatMap L.toChunks $$ 
      DCB.sinkFile "scratch/all"
    return soc

  listenSocket :: Socket -> Int -> IO EChan
  listenSocket soc bufSize = do
      chan <- atomically $ newTBMChan bufSize
      forkListener chan
      return chan
    where
      forkListener chan = void . forkIO $ listen soc 2 >> loop where 
        loop = do
          (conn, _) <- accept soc
          runResourceT $ sourceSocket conn $$ parseEvent =$ sinkTBMChan chan
          close conn
          loop

  parseEvent :: (MonadIO m, MonadResource m) => Conduit ByteString m E.Event
  parseEvent = 
      conduitParserEither json =$= awaitForever go
    where
      go (Left s) = error $ show s
      go (Right (_, msg)) = case (fromJSON msg :: Result E.Event) of
        Success a -> yield a
        Error s -> error s

  conduitSession :: (MonadIO m, MonadResource m) => Conduit E.Event m Rec.ConsoleRecord
  conduitSession = undefined