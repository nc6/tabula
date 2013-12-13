{-# LANGUAGE TemplateHaskell #-}
{-
Record data type. Designed to be imported qualified.
-}
module Tabula.Record where
  import Data.Aeson.TH
  import qualified Data.ByteString.Lazy as B
  import Data.Time.Clock

  type Env = [(String, String)]

  data Record = Record {
      command :: String
    , host :: String
    , priorEnv :: Env
    , posteriorEnv :: Env
    , startTime :: UTCTime
    , endTime :: UTCTime
    , stdout :: Maybe B.ByteString
    , stderr :: Maybe B.ByteString
    , exitStatus :: Int
  }

  $(deriveJSON defaultOptions ''Record)