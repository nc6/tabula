{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tabula.Options (
      options
    , command
    , verbosity
    , quiet
    , project
    , resume
    , db
    , Command(..)
    , Options
    , DefaultOptions
  ) where
  import Data.Char (toUpper)
  import Data.Vinyl

  import Options.Applicative hiding (command)
  import qualified Options.Applicative as Opt

  import System.Log (Priority(..))

  -------------- Options ------------------

  data Command = Default (PlainRec DefaultOptions)
                | Version

  command = Field :: "command" ::: Command

  type Options = "command" ::: Command ': CommonOptions
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
  type DefaultOptions = ["resume" ::: Bool, "project" ::: String, "db" ::: String]
  -- | Resume a session
  resume = Field :: "resume" ::: Bool

  --------------- Parsers ------------------

  version :: Parser (a -> a)
  version = infoOption "Tabula version 0.0.1"
    (  long "version"
    <> help "Print version information" )
  
  -- Shared options

  projectOption :: Parser String
  projectOption = argument str (metavar "PROJECT") 

  -- Option groups
  defaultOptions :: Rec DefaultOptions Parser
  defaultOptions = resume <-: (switch (long "resume" <> help "Resume existing session."))
                <+> project <-: projectOption
                <+> db <-: (strOption (long "destination"
                            <> short 'd'
                            <> metavar "DESTINATION"
                            <> value "$HOME/.tabula"
                            <> help "Directory to write logs to."))

  commonOptions :: Rec CommonOptions Parser
  commonOptions = verbosity <-: (nullOption (long "verbosity" 
                                            <> short 'V' 
                                            <> metavar "LEVEL"
                                            <> reader readPriority
                                            <> value ERROR
                                            <> help "Set logging level (default ERROR)"))
                <+> quiet <-: (switch (long "quiet" <> short 'q' <> help "Disable logging"))

  options = info (version <*> helper <*> commands <++> (dist commonOptions)) (
           header "tabula - a utility for recording shell sessions."
        <> progDesc "Open a recorded shell session for a specific project.")
    where
      commands = subparser (
          Opt.command "start" (
            info (fmap ((command =:) . Default) $ dist defaultOptions) 
              (progDesc "Start or resume a project session."))
          )
      (<++>) a b = liftA2 (<+>) a b

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