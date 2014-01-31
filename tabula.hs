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
  import Tabula.Destination (Destination, Project)
  import Tabula.Destination.File (fileDestination)
  import Tabula.Internal.Agent
  import Tabula.Options
  import Tabula.Shell (showShell)

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
    in showShell logDestination (rGet resume defOpts) (rGet bufferSize defOpts)

  ensureDataDir :: IO FilePath
  ensureDataDir = do
    workDir <- getAppUserDataDirectory "tabula"
    createDirectoryIfMissing False workDir
    return workDir