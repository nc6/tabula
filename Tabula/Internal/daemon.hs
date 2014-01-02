module Tabula.Internal.Daemon (daemon, BSChan) where

  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM.TBMChan (TBMChan())
  import Control.Monad (void)

  import Data.ByteString (ByteString)
  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import Data.Conduit.Network
  import Data.Conduit.TMChan (sourceTBMChan)

  import Network.Socket

  import System.Random (randomRIO)

  type BSChan = TBMChan ByteString

  daemon :: (BSChan, BSChan, BSChan) -> IO Socket
  daemon (inC, outC, errC) = do
    sockAddr <- fmap (\a -> "/tmp/tabula-" ++ (show a) ++ ".soc")
      (randomRIO (0, 9999999) :: IO Int)
    -- Start up a domain socket to do the listening
    soc <- socket AF_UNIX Stream 0
    bind soc (SockAddrUnix sockAddr)
    listenSocket soc "scratch/socket"
    _ <- forkIO $ runResourceT $ sourceTBMChan inC $$ DCB.sinkFile "scratch/stdin"
    _ <- forkIO $ runResourceT $ sourceTBMChan outC $$ DCB.sinkFile "scratch/stdout"
    _ <- forkIO $ runResourceT $ sourceTBMChan errC $$ DCB.sinkFile "scratch/stderr"
    return soc

  listenSocket :: Socket -> String -> IO ()
  listenSocket soc out = void . forkIO $ listen soc 2 >> loop where 
    loop = do
      (conn, _) <- accept soc
      runResourceT $ sourceSocket conn $$ DCB.sinkFile out
      close conn
      loop