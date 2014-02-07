module Tabula.Command.List where

  import Tabula.Destination

  list :: DestinationProvider -> IO ()
  list dp = listProjects dp >>= mapM_ (putStrLn . show)