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
module Tabula.Destination.File (
  fileProvider
) where
  import Control.Monad (filterM)

  import Data.Aeson
  import Data.Aeson.Encode.Pretty (encodePretty)
  import qualified Data.ByteString.Lazy as L
  import Data.Conduit
  import Data.Conduit.Attoparsec (conduitParser)
  import qualified Data.Conduit.Binary as DCB
  import qualified Data.Conduit.List as DCL
  import Data.Maybe (listToMaybe)
  
  import System.Directory
  import System.FilePath ((</>), takeFileName)
  import System.IO (hClose, openBinaryFile, IOMode(ReadMode, AppendMode))

  import Tabula.Destination
  import Tabula.Record

  projectFormat :: Project -> String
  projectFormat (GlobalProject key) = key
  projectFormat (UserProject _ key) = key

  fileProvider :: FilePath -> DestinationProvider
  fileProvider fp = DestinationProvider {
      listProjects = do
        entries <- getDirectoryContents fp
        files <- filterM doesFileExist . map (fp </>) $ entries
        return $ map (GlobalProject . takeFileName) files
    , projectDestination = fileDestination fp . projectFormat
    , removeProject = \project -> removeFile (fp </> projectFormat project)
  }

  -- | A file destination. Only supports appending records.
  fileDestination :: FilePath -> String -> Destination
  fileDestination fp proj = Destination {
      recordSink = DCL.map encodePretty =$= 
        DCL.concatMap L.toChunks =$= bracketP 
                        (openBinaryFile (fp </> proj) AppendMode) 
                        hClose 
                        DCB.sinkHandle
    , getLastRecord = (runResourceT $ fileSource fp proj $$ DCL.consume) >>= 
        return . listToMaybe . reverse
    , recordSource = fileSource fp proj          
  }

  fileSource :: FilePath -> String -> Source (ResourceT IO) Record
  fileSource fp proj = bracketP
      (openBinaryFile (fp </> proj) ReadMode)
      hClose
      DCB.sourceHandle =$= parseRec
    where
      parseRec = conduitParser json =$= awaitForever go
      go (_, rec) = case (fromJSON rec :: Result Record) of
        Success a -> yield a
        Error s -> error s