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
    unless (rGet quiet opts) $ do
      logFile <- fileHandler (workDir ++ "/log") (rGet verbosity opts)
      updateGlobalLogger "tabula" (
        setLevel (rGet verbosity opts) . setHandlers [logFile])
    case (rGet command opts) of
      Version -> putStrLn "Version 0.1"
      Default defOpts -> startProject workDir defOpts

  startProject :: FilePath -> PlainRec DefaultOptions -> IO ()
  startProject workDir defOpts = do
    let logDestination = (fromMaybe workDir (rGet db defOpts)) 
                            ++ "/" ++ (rGet project defOpts)
    showShell logDestination (rGet bufferSize defOpts)

  ensureDataDir :: IO FilePath
  ensureDataDir = do
    workDir <- getAppUserDataDirectory "tabula"
    createDirectoryIfMissing False workDir
    return workDir
