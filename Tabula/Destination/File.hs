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
  import Control.Applicative ((<$>))
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
  import System.Posix.Files
  import System.Posix.IO (handleToFd, fdToHandle)
  import System.Posix.Types (FileMode)
  import System.Posix.User (getEffectiveUserName)

  import Tabula.Destination
  import Tabula.Record

  projectFormat :: Project -> String
  projectFormat (GlobalProject key) = key
  projectFormat (UserProject _ key) = key

  projectPermissions :: Project -> FileMode
  projectPermissions (GlobalProject _) = foldl1 unionFileModes $ 
    [
        ownerReadMode, ownerWriteMode
      , groupReadMode, groupWriteMode
      , otherReadMode, otherWriteMode
    ]
  projectPermissions (UserProject _ _) = foldl1 unionFileModes $ 
    [ownerReadMode, ownerWriteMode]

  fileProvider :: FilePath -> DestinationProvider
  fileProvider fp = let
      readProject file = do
        mode <-  fileMode <$> (getFileStatus file)
        username <- getEffectiveUserName
        return $ case (intersectFileModes mode groupReadMode) of
          a | a == nullFileMode -> UserProject username (takeFileName file)
          a | a == groupReadMode -> GlobalProject (takeFileName file)
    in DestinationProvider {
        listProjects = do
          entries <- getDirectoryContents fp
          files <- filterM doesFileExist . map (fp </>) $ entries
          mapM readProject files
      , projectDestination = fileDestination fp
      , removeProject = \project -> removeFile (fp </> projectFormat project)
    }

  -- | A file destination.
  fileDestination :: FilePath -> Project -> Destination
  fileDestination fp proj = let 
      projName = projectFormat proj
      initFile = do
        h <- (openBinaryFile (fp </> projName) AppendMode)
        fd <- handleToFd h
        setFdMode fd $ projectPermissions proj
        fdToHandle fd
    in Destination {
        recordSink = DCL.map encodePretty =$= 
          DCL.concatMap L.toChunks =$= bracketP 
                          initFile
                          hClose 
                          DCB.sinkHandle
      , getLastRecord = (runResourceT $ fileSource fp projName $$ DCL.consume) >>= 
          return . listToMaybe . reverse
      , recordSource = fileSource fp projName          
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