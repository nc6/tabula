{-
Copyright (c) 2014 Genome Research Ltd.

Author: Nicholas A. Clarke <nicholas.clarke@sanger.ac.uk>

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE LambdaCase,RankNTypes #-}
module Tabula.Internal.Daemon (daemon, BSChan) where

  import Control.Concurrent (forkIO, forkFinally)
  import Control.Concurrent.MVar (MVar(), newEmptyMVar, putMVar)
  import Control.Concurrent.STM (atomically)
  import Control.Concurrent.STM.TBMChan (newTBMChan, TBMChan())
  import Control.Monad (unless, void)
  import Control.Monad.IO.Class (MonadIO (liftIO))

  import Data.Aeson (json, fromJSON, Result(..))
  import Data.ByteString (ByteString)
  import qualified Data.ByteString as B
  import qualified Data.ByteString.Lazy as L
  import Data.Conduit
  import Data.Conduit.Attoparsec (conduitParserEither)
  import qualified Data.Conduit.List as DCL
  import Data.Conduit.TMChan (sinkTBMChan, sourceTBMChan, mergeSources)
  import Data.Time.Clock

  import Network.BSD (getHostName)
  import Network.Socket hiding (recv)
  import Network.Socket.ByteString (recv)

  import System.Log.Logger
  import System.Random (randomRIO)

  import Tabula.Destination
  import qualified Tabula.Internal.Event as E
  import Tabula.Record (record)
  import qualified Tabula.Record.Console as Rec
  import qualified Tabula.Record.Environment as Env

  {- | Control channel to allow the main thread to control the conduit. At
       the moment this simply is used to send a single 'shutdown' message when
       the shell closes.
  -}
  type FlagChan = TBMChan ()
  type EChan = TBMChan E.Event
  type BSChan = TBMChan ByteString

  daemon :: Destination -- ^ Path to the file to write out to.
         -> (BSChan, BSChan, BSChan, FlagChan) -- ^ Channels (in, out, err, control)
         -> [String] -- Ignored commands
         -> Int -- ^ Buffer size - maybe should just make this a function somewhere?
         -> IO (MVar (), Socket) -- ^ closing mvar, socket
  daemon destination (inC, outC, errC, flagC) ignoredCommands bufSize = do
    host <- getHostName
    debugM "tabula.daemon" $ "Host name: " ++ host
    sockAddr <- fmap (\a -> "/tmp/tabula-" ++ show a ++ ".soc")
      (randomRIO (0, 9999999) :: IO Int)
    mapM_ (\i -> debugM "tabula.daemon" $ "Ignored command: " ++ i) $ 
      ignoredCommands
    -- Start up a domain socket to do the listening
    soc <- socket AF_UNIX Stream 0
    bind soc (SockAddrUnix sockAddr)
    socChan <- listenSocket soc bufSize
    -- Merge all channels together
    let inS = sourceTBMChan inC $= DCL.map E.Stdin
        outS = sourceTBMChan outC $= DCL.map E.Stdout
        errS = sourceTBMChan errC $= DCL.map E.Stderr
        stopS = sourceTBMChan flagC $= DCL.map (\() -> E.Stop)
        socS = sourceTBMChan socChan
        mergedS = mergeSources [inS, outS, errS, stopS, socS] bufSize
    debugM "tabula.daemon" $ "Merged all sources."
    -- Sink to a session list
    x <- newEmptyMVar
    let cmdFilter e = elem e $ ignoredCommands
        fileSink = recordSink destination
    _ <- forkFinally (runResourceT $ mergedS >>= 
      \a -> a $$ conduitSession bufSize host cmdFilter =$= 
        DCL.map (record) =$=
        fileSink) (\_ -> putMVar x ())
    return (x, soc)

  listenSocket :: Socket -> Int -> IO EChan
  listenSocket soc bufSize = do
      chan <- atomically $ newTBMChan bufSize
      forkListener chan
      return chan
    where
      forkListener chan = void . forkIO . runResourceT $ 
          (sourceSocket soc $$ parseEvent =$ sinkTBMChan chan True) 
      sourceSocket :: (MonadIO m, MonadResource m) => Socket -> Producer m ByteString
      sourceSocket sock = bracketP
          (listen sock 1)
          (\() -> close $ sock)
          (\() -> loop)
        where
          loop = (liftIO $ isListening sock) >>= \case
            True -> do 
              (conn, _) <- liftIO $ accept sock
              loop' conn
              liftIO $ close conn
              loop
            False -> return ()
          loop' conn = do
            bs <- liftIO $ recv conn 4096
            if B.null bs
              then return ()
              else yield bs >> loop' conn

  parseEvent :: (MonadIO m) => Conduit ByteString m E.Event
  parseEvent = 
      conduitParserEither json =$= awaitForever go
    where
      go (Left s) = error $ show s
      go (Right (_, msg)) = case (fromJSON msg :: Result E.Event) of
        Success a -> yield a
        Error s -> error s

  data SessionState = Session {
      ssIn :: [ByteString]
    , ssOut :: [ByteString]
    , ssErr :: [ByteString]
    , ssTrap :: [Rec.ConsoleEvent]
    , ssPromptStart :: UTCTime
    , ssInitialEnv :: Env.Env
    , ssWd :: FilePath
    , ssCurrentEnv :: Env.Env
  } | NoSession

  -- | Sessionise the event stream to a stream of console records.
  conduitSession :: (MonadIO m, MonadResource m) => 
    Int -> String -> (String -> Bool) -> Conduit E.Event m Rec.ConsoleRecord
  conduitSession bufSize host cmdFilter = go NoSession
    where
      go NoSession = await >>= \case
        -- No session, only care about prompt events
        Just (E.Prompt st env cwd _ _) ->
          go $ Session [] [] [] [] st env cwd env
        Just (E.Stop) -> return ()
        _ -> go NoSession
      go ss = await >>= \case
        Just (E.Stdin sb) -> let 
            inB = ssIn ss
            inB' = if (length inB >= bufSize) then inB else sb : inB
          in go $ ss { ssIn = inB' }
        Just (E.Stdout sb) -> let
            outB = ssOut ss 
            outB' = if (length outB >= bufSize) then outB else sb : outB
          in go $ ss { ssOut = outB' }
        Just (E.Stderr sb) -> let
            errB = ssErr ss
            errB' = if (length errB >= bufSize) then errB else sb : errB
          in go $ ss { ssErr = errB' }
        Just (sb @ (E.Debug _ cmd _ _ env)) -> let
            trapB = ssTrap ss
            debugEvent = mkEvent sb (ssCurrentEnv ss)
          in go $ case cmd of
            x | cmdFilter x -> ss
            _ -> ss {ssTrap = (debugEvent : trapB),  ssCurrentEnv = env }
        Just (sb @ (E.Prompt _ _ _ _ _)) -> 
            sessionize ss sb
        Just (E.Stop) -> return ()
        Nothing -> return ()

      sessionize ss sb = let
            stdin = B.concat . reverse $ (ssIn ss)
            stdout = B.concat . reverse $ (ssOut ss)
            stderr = B.concat . reverse $ (ssErr ss)
            E.Prompt endTime posteriorEnv nwd command exitStatus = sb
            events = reverse (ssTrap ss)
            startTime = case events of
              h : _ -> Rec.timestamp h
              _ -> (ssPromptStart ss)
            rec = (Rec.ConsoleRecord
                command
                host
                (ssWd ss)
                (ssInitialEnv ss)
                (Env.diff (ssInitialEnv ss) posteriorEnv)
                startTime
                endTime
                (L.fromStrict stdin)
                (L.fromStrict stdout)
                (L.fromStrict stderr)
                exitStatus
                events 
             )
          in (unless (recordFilter rec) $ yield rec)
            >> (go $ Session [] [] [] [] endTime posteriorEnv nwd posteriorEnv)
          where
            recordFilter = orF [
                  cmdFilter . Rec.command
                , null . Rec.events
              ]
            (|||) a b = \c -> if a c then True else b c
            orF = foldl1 (|||)

      mkEvent (E.Debug time cmd pid ppid environment) oldEnv = 
        Rec.ConsoleEvent time cmd pid ppid (Env.diff oldEnv environment)