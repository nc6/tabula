module Tabula.Strings where

  metaCommandNotFound :: String -> String
  metaCommandNotFound a = "Metacommand " ++ a ++ 
    " not implemented. Use :help to get a list of commands."