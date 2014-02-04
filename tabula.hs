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
{-# LANGUAGE LambdaCase #-}
module Main where
  import Control.Monad (unless)

  import Data.Maybe (fromMaybe)
  import Data.Vinyl

  import Options.Applicative (execParser)

  import System.Directory
  import System.Environment (getArgs)
  import System.Log.Logger
  import System.Log.Handler.Simple (fileHandler)

  import Tabula.Command.Cat
  import Tabula.Command.Record
  import Tabula.Destination (Destination, Project)
  import Tabula.Destination.File (fileDestination)
  import Tabula.Internal.Agent
  import Tabula.Options

  main :: IO ()
  main = getArgs >>= \case
    "trap" : rest -> trap rest
    "prompt" : rest -> prompt rest
    _ -> execParser options >>= run

  run :: PlainRec Options -> IO ()
  run opts = do
    workDir <- ensureDataDir
    let defaultDestination = fileDestination workDir
    unless (rGet quiet opts) $ do
      logFile <- fileHandler (workDir ++ "/log") (rGet verbosity opts)
      updateGlobalLogger "tabula" (
        setLevel (rGet verbosity opts) . setHandlers [logFile])
    case (rGet command opts) of
      Record recOpts -> startProject defaultDestination recOpts
      Cat catOpts -> catSession $ (fromMaybe defaultDestination (rGet db catOpts)) 
                          (rGet project catOpts)

  startProject :: (Project -> Destination) -> PlainRec RecordOptions -> IO ()
  startProject defaultDestination defOpts = let
      logDestination = (fromMaybe defaultDestination (rGet db defOpts)) 
                          (rGet project defOpts)
    in record logDestination (rGet resume defOpts) (rGet bufferSize defOpts)

  ensureDataDir :: IO FilePath
  ensureDataDir = do
    workDir <- getAppUserDataDirectory "tabula"
    createDirectoryIfMissing False workDir
    return workDir