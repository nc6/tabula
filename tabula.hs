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
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase, UnicodeSyntax #-}
module Main where
  import Control.Monad (unless)

  import Data.Maybe (fromMaybe)
  import Data.Vinyl
  import Data.Vinyl.Unicode

  import Options.Applicative (execParser)

  import System.Directory
  import System.Environment (getArgs)
  import System.FilePath ((</>))
  import System.Log.Logger
  import System.Log.Handler.Simple (fileHandler)
  import System.Posix.User (getLoginName)

  import Tabula.Command.Cat
  import Tabula.Command.List
  import Tabula.Command.Record
  import Tabula.Destination (Project(..), DestinationProvider, projectDestination)
  import Tabula.Destination.File (fileProvider)
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
      username <- getLoginName
      let projectDir = workDir </> "projects"
          defaultDestination = fileProvider projectDir
      createDirectoryIfMissing False projectDir
      unless (rGet quiet opts) $ do
        logFile <- fileHandler (workDir ++ "/log") (rGet verbosity opts)
        updateGlobalLogger "tabula" (
          setLevel (rGet verbosity opts) . setHandlers [logFile])
      case (rGet command opts) of
        Record recOpts -> 
          record dest (rGet resume recOpts) (rGet bufferSize recOpts) where
            dest = projectDestination (dp defaultDestination recOpts) $ 
              proj username recOpts
        Cat catOpts -> catSession dest fmt where
          dest = projectDestination (dp defaultDestination catOpts) $
            proj username catOpts
          fmt = rGet showAsHistory catOpts
        List listOpts -> list $ dp defaultDestination listOpts
    where
      dp :: (T_db ∈ fields) => DestinationProvider -> PlainRec fields -> DestinationProvider
      dp dflt fields = (fromMaybe dflt (rGet db fields))
      proj :: (T_project ∈ fields, T_global ∈ fields) => String -> PlainRec fields -> Project
      proj un fields = let
          p = rGet project fields
          g = rGet global fields
        in if g then GlobalProject p else UserProject un p

  ensureDataDir :: IO FilePath
  ensureDataDir = do
    workDir <- getAppUserDataDirectory "tabula"
    createDirectoryIfMissing False workDir
    return workDir