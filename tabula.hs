{-# LANGUAGE LambdaCase #-}
module Main where
 
  import System.Environment (getArgs)
  import System.Log.Logger
  import System.Log.Handler.Simple (fileHandler)

  import Tabula.Internal.Agent
  import Tabula.Shell (showShell)

  main :: IO ()
  main = getArgs >>= \case
    "trap" : rest -> trap rest
    "prompt" : rest -> prompt rest
    _ -> do
      logFile <- fileHandler "scratch/log" DEBUG
      updateGlobalLogger "tabula" (setLevel DEBUG . setHandlers [logFile]) 
      showShell 64