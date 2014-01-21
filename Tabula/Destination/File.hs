module Tabula.Destination.File where
  import Data.Aeson.Encode.Pretty (encodePretty)
  import qualified Data.ByteString.Lazy as L
  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import qualified Data.Conduit.List as DCL
  
  --import System.Directory
  import System.IO (hClose, openBinaryFile, IOMode(AppendMode))

  import Tabula.Destination

  -- | A file destination. Only supports appending records.
  fileDestination :: FilePath -> Destination
  fileDestination fp = Destination {
      recordSink = DCL.map encodePretty =$= 
        DCL.concatMap L.toChunks =$= bracketP 
                        (openBinaryFile fp AppendMode) 
                        hClose 
                        DCB.sinkHandle
    , getLastRecord = undefined
    , recordSource = undefined
  }

  directoryDestination :: FilePath -> String -> Destination
  directoryDestination dir proj = Destination {
      recordSink = undefined
    , getLastRecord = undefined
    , recordSource = undefined
  }