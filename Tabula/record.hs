{-# LANGUAGE TemplateHaskell #-}
{-
Record data type. Designed to be imported qualified.
-}
module Tabula.Record where
  import Data.Aeson
  import Data.Aeson.TH
  import Data.Time.Clock

  data Record = Record {
      namespace :: String
    , version :: Int
    , timestamp :: UTCTime
    , entry :: Value
  }

  $(deriveJSON defaultOptions ''Record)

  class (ToJSON a, FromJSON a) => Recordable a where
    getNamespace :: a -> String
    getVersion :: a -> Int
    getTimestamp :: a -> UTCTime

  record :: (Recordable a) => a -> Record
  record a = Record 
                (getNamespace a)
                (getVersion a)
                (getTimestamp a)
                (toJSON a)