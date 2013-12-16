{-# LANGUAGE TemplateHaskell #-}
module Tabula.Record.Console where
  import Data.Aeson.TH
  import qualified Data.ByteString.Lazy as B
  import Data.Time.Clock

  import Tabula.Record

  type Env = [(String, String)]

  data ConsoleRecord = ConsoleRecord {
      command :: String
    , host :: String
    , workingDirectory :: FilePath
    , priorEnv :: Env
    , posteriorEnv :: Env
    , startTime :: UTCTime
    , endTime :: UTCTime
    , stdout :: Maybe B.ByteString
    , stderr :: Maybe B.ByteString
    , exitStatus :: Int
  }

  $(deriveJSON defaultOptions ''ConsoleRecord)

  instance Recordable ConsoleRecord where
    getNamespace _ = "tabula::consoleRecord"
    getVersion _ = 2
    getTimestamp a = startTime a