{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tabula.Options where
  import Data.Char (toUpper)
  import Data.Vinyl

  import Options.Applicative

  import System.Log (Priority(..))

  -------------- Options ------------------

  -- Common options --
  type CommonOptions = ["verbosity" ::: Priority, "quiet" ::: Bool]
  -- | Verbosity (logging level)
  verbosity = Field :: "verbosity" ::: Priority
  -- | Quiet mode (disable all logging)
  quiet = Field :: "quiet" ::: Bool

  -- Shared options --
  -- | Specify which project 
  project = Field :: "project" ::: String
  -- | Project database. At the moment, this is just a directory.
  db = Field :: "db" ::: String

  -- Default options --
  type DefaultOptions = ["resume" ::: Bool, "project" ::: String]
  -- | Resume a session
  resume = Field :: "resume" ::: Bool

  --------------- Parsers ------------------

  -- | Print version information.
  version :: Parser (a -> a)
  version = infoOption "0.1" ( long "version" <> help "Print version information.")

  projectOption :: Parser String
  projectOption = argument str (metavar "PROJECT") 

  defaultOptions :: Rec DefaultOptions Parser
  defaultOptions = resume <-: (switch (long "resume" <> help "Resume existing session."))
                <+> project <-: projectOption

  commonOptions :: Rec CommonOptions Parser
  commonOptions = verbosity <-: (nullOption (long "verbosity" 
                                            <> short 'V' 
                                            <> metavar "LEVEL"
                                            <> reader readPriority
                                            <> value ERROR
                                            <> help "Set logging level (default ERROR)"))
                <+> quiet <-: (switch (long "quiet" <> short 'q' <> help "Disable logging"))

  --------------- Utility -------------------

  readPriority :: Monad m => String -> m Priority
  readPriority p = case map toUpper p of
    "DEBUG" -> return DEBUG
    "INFO" -> return INFO
    "NOTICE" -> return NOTICE
    "WARNING" -> return WARNING
    "ERROR" -> return ERROR
    "CRITICAL" -> return CRITICAL
    "ALERT" -> return ALERT
    "EMERGENCY" -> return EMERGENCY 
    x -> fail $ "Invalid logging level specified: " ++ x