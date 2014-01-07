{-# LANGUAGE TemplateHaskell #-}
module Tabula.Record.Console where
  import Data.Aeson.TH
  import qualified Data.ByteString.Lazy as B
  import Data.Time.Clock

  import Tabula.Record
  import qualified Tabula.Record.Environment as Env

  data ConsoleEvent = ConsoleEvent {
        timestamp :: UTCTime
      , subCommand :: String
      , pid :: Int
      , ppid :: Int
      , env :: Env.EnvChange
  } deriving (Eq, Show)

  $(deriveJSON defaultOptions ''ConsoleEvent)

  data ConsoleRecord = ConsoleRecord {
      command :: String
    , host :: String
    , workingDirectory :: FilePath
    , priorEnv :: Env.Env
    , posteriorEnv :: Env.Env
    , startTime :: UTCTime
    , endTime :: UTCTime
    , stdin :: B.ByteString
    , stdout :: B.ByteString
    , stderr :: B.ByteString
    , exitStatus :: Int
    , events :: [ConsoleEvent]
  } deriving (Eq, Show)

  $(deriveJSON defaultOptions ''ConsoleRecord)

  instance Recordable ConsoleRecord where
    getNamespace _ = "tabula::consoleRecord"
    getVersion _ = 5
    getTimestamp = startTime