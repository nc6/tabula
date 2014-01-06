{-# LANGUAGE LambdaCase #-}
module Tabula.Internal.Daemon (daemon, BSChan) where

  import Control.Arrow ((>>>))
  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM (atomically)
  import Control.Concurrent.STM.TBMChan (newTBMChan, TBMChan())
  import Control.Monad (void)
  import Control.Monad.IO.Class

  import Data.Aeson (encode, json, fromJSON, Result(..))
  import Data.ByteString (ByteString)
  import qualified Data.ByteString as B
  import qualified Data.ByteString.Lazy as L
  import Data.Conduit
  import Data.Conduit.Attoparsec (conduitParserEither)
  import qualified Data.Conduit.Binary as DCB
  import qualified Data.Conduit.List as DCL
  import Data.Conduit.Network
  import Data.Conduit.TMChan (sinkTBMChan, sourceTBMChan, mergeSources)
  import Debug.Trace (trace, traceShow)

  import Network.BSD (getHostName)
  import Network.Socket

  import System.Log.Logger
  import System.Random (randomRIO)

  import qualified Tabula.Internal.Event as E
  import Tabula.Record (record)
  import qualified Tabula.Record.Console as Rec

  type EChan = TBMChan E.Event
  type BSChan = TBMChan ByteString

  daemon :: (BSChan, BSChan, BSChan) -> Int -> IO Socket
  daemon (inC, outC, errC) bufSize = do
    host <- getHostName
    debugM "tabula.daemon" $ "Host name: " ++ host
    sockAddr <- fmap (\a -> "/tmp/tabula-" ++ show a ++ ".soc")
      (randomRIO (0, 9999999) :: IO Int)
    -- Start up a domain socket to do the listening
    soc <- socket AF_UNIX Stream 0
    bind soc (SockAddrUnix sockAddr)
    socChan <- listenSocket soc bufSize
    -- Merge all channels together
    let inS = sourceTBMChan inC $= DCL.map E.Stdin
        outS = sourceTBMChan outC $= DCL.map E.Stdout
        errS = sourceTBMChan errC $= DCL.map E.Stderr
        socS = sourceTBMChan socChan
        mergedS = mergeSources [inS, outS, errS, socS] bufSize
    debugM "tabula.daemon" $ "Merged all sources."
    -- Sink to a session list
    _ <- forkIO . runResourceT $ mergedS >>= 
      \a -> a $$ conduitSession bufSize host =$= 
        DCL.map (encode . record) =$=
        DCL.concatMap L.toChunks =$= 
        DCB.sinkFile "scratch/all"
    return soc

  listenSocket :: Socket -> Int -> IO EChan
  listenSocket soc bufSize = do
      chan <- atomically $ newTBMChan bufSize
      forkListener chan
      return chan
    where
      forkListener chan = void . forkIO $ listen soc 1 >> loop where 
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

  traceId :: (Show a) => a -> a
  traceId a = traceShow a a 

  -- | Sessionise the event stream to a stream of console records.
  conduitSession :: (MonadIO m, MonadResource m) => 
    Int -> String -> Conduit E.Event m Rec.ConsoleRecord
  conduitSession bufSize host = go [] [] [] [] Nothing
    where
      go inB outB errB trapB oldP = await >>= (traceId >>> \case
        Just (E.Stdin sb) -> let 
            inB' = if (length inB >= bufSize) then inB else sb : inB
          in go inB' outB errB trapB oldP
        Just (E.Stdout sb) -> let 
            outB' = if (length outB >= bufSize) then outB else sb : outB
          in go inB outB' errB trapB oldP
        Just (E.Stderr sb) -> let 
            errB' = if (length errB >= bufSize) then errB else sb : errB
          in go inB outB errB' trapB oldP
        Just (sb @ (E.Debug _ _ _ _ _)) -> 
            go inB outB errB (sb : trapB) oldP
        Just (sb @ (E.Prompt _ _ _ _ _)) -> 
            sessionize inB outB errB trapB sb oldP
        Nothing -> return ())

      sessionize inB outB errB trapB prompt old = case old of
        Nothing -> trace "No previous prompt." $ go [] [] [] [] (Just prompt)
        Just prev -> trace "Previous prompt." $ let
            stdin = B.concat inB
            stdout = B.concat outB
            stderr = B.concat errB
            E.Prompt badStartTime priorEnv cwd _ _ = prev
            E.Prompt endTime posteriorEnv _ command exitStatus = prompt
            events = map mkEvent . reverse $ trapB
            startTime = case events of
              h : _ -> Rec.timestamp h
              _ -> badStartTime
            rec = (Rec.ConsoleRecord
                command
                host
                cwd
                priorEnv
                posteriorEnv
                startTime
                endTime
                (L.fromStrict stdin)
                (L.fromStrict stdout)
                (L.fromStrict stderr)
                exitStatus
                events 
             )
          in trace (show rec) $ yield rec >> go [] [] [] [] (Just prompt)

      mkEvent (E.Debug time cmd pid ppid environment) = 
        Rec.ConsoleEvent time cmd pid ppid environment