{-# LANGUAGE LambdaCase #-}
module Main where
 
  import System.Directory
  import System.Environment (getArgs)
  import System.Log.Logger
  import System.Log.Handler.Simple (fileHandler)

  import Tabula.Internal.Agent
  import Tabula.Shell (showShell)

  main :: IO ()
  main = getArgs >>= \case
    "trap" : rest -> trap rest
    "prompt" : rest -> prompt rest
    name : [] -> startProject name
    [] -> startProject "default"

  startProject :: String -> IO ()
  startProject proj = do
    workDir <- ensureWorkDir
    logFile <- fileHandler (workDir ++ "/log.debug") DEBUG
    updateGlobalLogger "tabula" (setLevel DEBUG . setHandlers [logFile]) 
    showShell (workDir ++ "/" ++ proj) 64

  ensureWorkDir :: IO FilePath
  ensureWorkDir = do
    workDir <- getAppUserDataDirectory "tabula"
    createDirectoryIfMissing False workDir
    return workDir
