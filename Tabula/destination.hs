{-# LANGUAGE RankNTypes #-}
module Tabula.Destination where
  import Tabula.Record

  data Destination = Destination {
    makeEntry :: Record -> IO ()
  }