module Tabula.Destination.File where
  import Data.Aeson.Encode.Pretty (encodePretty)
  import qualified Data.ByteString.Lazy as L
  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import qualified Data.Conduit.List as DCL
  
  --import System.Directory
  import System.FilePath ((</>))
  import System.IO (hClose, openBinaryFile, IOMode(AppendMode))

  import Tabula.Destination

  -- | A file destination. Only supports appending records.
  fileDestination :: FilePath -> String -> Destination
  fileDestination fp proj = Destination {
      recordSink = DCL.map encodePretty =$= 
        DCL.concatMap L.toChunks =$= bracketP 
                        (openBinaryFile (fp </> proj) AppendMode) 
                        hClose 
                        DCB.sinkHandle
    , getLastRecord = return Nothing
    , recordSource = undefined
  }