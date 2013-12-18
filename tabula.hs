module Main where
  import Prelude hiding (sequence)

  import Data.List (isPrefixOf)

  import Network.URL

  import qualified System.Console.Readline as RL

  import Tabula.Exec
  import qualified Tabula.Destination.File as FD
  import qualified Tabula.Strings as S

  data Destination = File FilePath 
                   | Port Int
                   | HTTP URL

  data Options = Options {
    optDestination :: Destination
  }

  main :: IO ()
  main = readEvalPrintLoop

  readEvalPrintLoop :: IO ()
  readEvalPrintLoop = do
      maybeLine <- RL.readline "% "
      case maybeLine of
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just mc | isPrefixOf ":" mc -> do
          evalMetaCommand (drop 1 mc)
          readEvalPrintLoop
        Just line -> do 
          RL.addHistory line
          exec (FD.fileDestination "typescript") line
          readEvalPrintLoop

  evalMetaCommand :: String -> IO ()
  evalMetaCommand command = case command of
    a -> putStrLn $ S.metaCommandNotFound a