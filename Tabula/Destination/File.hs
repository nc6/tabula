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