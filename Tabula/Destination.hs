{-# LANGUAGE RankNTypes #-}
module Tabula.Destination where
  import Data.Conduit
  import Tabula.Record

  type Project = String
  
  data Destination = Destination {
      recordSink :: Sink Record (ResourceT IO) () -- ^ Sink records to a store
    , getLastRecord :: IO (Maybe Record) -- ^ Fetch the last
    , recordSource :: Source (ResourceT IO) Record -- ^ Get a stream of all records, first to last
  }