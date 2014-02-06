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
{-# LANGUAGE RankNTypes #-}
module Tabula.Destination where
  import Data.Conduit
  import Tabula.Record

  data Project = UserProject String String
               | GlobalProject String

  data DestinationProvider = DestinationProvider {
      listProjects :: IO [Project]
    , projectDestination :: Project -> Destination
    , removeProject :: Project -> IO ()
  }
  
  data Destination = Destination {
      recordSink :: Sink Record (ResourceT IO) () -- ^ Sink records to a store
    , getLastRecord :: IO (Maybe Record) -- ^ Fetch the last
    , recordSource :: Source (ResourceT IO) Record -- ^ Get a stream of all records, first to last
  }