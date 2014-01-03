{-# LANGUAGE TemplateHaskell #-}
module Tabula.Record.Console where
  import Data.Aeson.TH
  import qualified Data.ByteString.Lazy as B
  import Data.Time.Clock

  import Tabula.Record

  type Env = [(String, String)]

  data ConsoleEvent = ConsoleEvent {
        timestamp :: UTCTime
      , subCommand :: String
      , pid :: Int
      , ppid :: Int
      , env :: Env
  }

  $(deriveJSON defaultOptions ''ConsoleEvent)

  data ConsoleRecord = ConsoleRecord {
      command :: String
    , host :: String
    , workingDirectory :: FilePath
    , priorEnv :: Env
    , posteriorEnv :: Env
    , startTime :: UTCTime
    , endTime :: UTCTime
    , stdin :: B.ByteString
    , stdout :: B.ByteString
    , stderr :: B.ByteString
    , exitStatus :: Int
    , events :: [ConsoleEvent]
  }

  $(deriveJSON defaultOptions ''ConsoleRecord)

  instance Recordable ConsoleRecord where
    getNamespace _ = "tabula::consoleRecord"
    getVersion _ = 4
    getTimestamp = startTime