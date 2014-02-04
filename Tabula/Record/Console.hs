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
      , envChanges :: Env.EnvChange
  } deriving (Eq, Show)

  $(deriveJSON defaultOptions ''ConsoleEvent)

  data ConsoleRecord = ConsoleRecord {
      command :: String
    , host :: String
    , workingDirectory :: FilePath
    , priorEnv :: Env.Env
    , posteriorEnv :: Env.EnvChange
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
    getVersion _ = 6
    getTimestamp = startTime