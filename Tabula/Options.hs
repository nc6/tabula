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
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tabula.Options (
      options
    , command
    , verbosity
    , quiet
    , project
    , resume
    , db
    , bufferSize
    , global
    , Command(..)
    , Options
    , RecordOptions
    , CatOptions
    , T_project
    , T_db
    , T_global
    , readDestination
    , readProjectName
    , showAsHistory
  ) where
  import Data.Char (toUpper)
  import Data.Version (showVersion)
  import Data.Vinyl
  import Database.Redis (PortID(PortNumber))

  import Options.Applicative hiding (command)
  import qualified Options.Applicative as Opt

  import System.Log (Priority(..))

  import Tabula.Command.Cat (Format(..))
  import Tabula.Destination
  import Tabula.Destination.File
  import Tabula.Destination.Redis
  import qualified Text.Parsec as P

  import qualified Paths_tabula as Paths (version)

  -------------- Options ------------------

  data Command = Record (PlainRec RecordOptions)
               | Cat (PlainRec CatOptions)
               | List (PlainRec ListOptions)

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
  type T_project = "project" ::: String
  project = Field :: T_project

  -- | Project database.
  type T_db = "db" ::: Maybe DestinationProvider
  db = Field :: T_db

  -- | Use global namespace?
  type T_global = "global" ::: Bool
  global = Field :: T_global

  -- Default options --
  type RecordOptions = [ "resume" ::: Bool
                          , T_db
                          , "bufferSize" ::: Int
                          , T_project
                          , T_global
                        ]
  -- | Resume a session
  resume = Field :: "resume" ::: Bool
  -- | Set buffer size
  bufferSize = Field :: "bufferSize" ::: Int

  -- | Cat options
  type T_showAsHistory = "showAsHistory" ::: Format
  showAsHistory = Field :: T_showAsHistory

  type CatOptions = [ T_db, T_project, T_showAsHistory, T_global ]

  type ListOptions = '[T_db]

  --------------- Parsers ------------------

  version :: Parser (a -> a)
  version = infoOption ("Tabula version " ++ showVersion Paths.version)
    (  long "version"
    <> help "Print version information" )
  
  -- Shared options

  projectOption :: Parser String
  projectOption = argument readProjectName (metavar "PROJECT" <> value "default")

  destinationOption :: String -> Parser DestinationProvider
  destinationOption helpText = nullOption (long "destination"
                            <> short 'd'
                            <> metavar "DESTINATION"
                            <> reader readDestination
                            <> help helpText)

  globalOption :: Parser Bool
  globalOption = switch (long "global"
                         <> short 'g'
                         <> help "Use global rather than user namespace.")

  -- Option groups
  recordOptions :: Rec RecordOptions Parser
  recordOptions = resume <-: (switch (long "resume" <> help "Resume existing session."))
                <+> db <-: optional (destinationOption "Destination to write logs to.")
                <+> bufferSize <-: option (long "bufferSize"
                                             <> metavar "SIZE"
                                             <> value 64
                                             <> help "Set buffer size (in multiples of 4096B)")
                <+> project <-: projectOption
                <+> global <-: globalOption

  catOptions :: Rec CatOptions Parser
  catOptions = db <-: optional (destinationOption "Destination to read logs from.")
              <+> project <-: projectOption
              <+> showAsHistory <-: (flag Full AsHistory (long "as-history" 
                            <> help "Show in bash history format (e.g. only commands)"))
              <+> global <-: globalOption

  listOptions :: Rec ListOptions Parser
  listOptions = db <-: optional (destinationOption "Destination to list projects.")

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
              info (fmap ((command =:) . Record) $ dist recordOptions) 
                (progDesc "Start or resume a project session."))
            <> Opt.command "cat" (
              info (fmap ((command =:) . Cat) $ dist catOptions)
                (progDesc "Print a project session to stdout."))
            <> Opt.command "ls" (
              info (fmap ((command =:) . List) $ dist listOptions)
                (progDesc "List all projects created at a destination."))
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

  readProjectName :: Monad m => String -> m String
  readProjectName s = case P.parse projectNameParser "Project name" s of
      Left err -> fail $ "Invalid project name: " ++ show err
      Right x -> return x

  readDestination :: Monad m => String -> m DestinationProvider
  readDestination s = let
      protoSep = P.string "://"
      path = P.many1 (P.noneOf ":")
      fileDest = P.string "file" >> protoSep >> do
        p <- path
        return $ fileProvider p
      redisDest = P.string "redis" >> protoSep >> do
        host <- P.option (connectHost defaultConnectInfo) $ 
          P.many1 (P.alphaNum <|> P.char '.')
        port <- P.option (connectPort defaultConnectInfo) $ 
          liftA (PortNumber . fromIntegral . readInt) (P.char ':' >> P.many1 (P.digit))
        let connInfo = defaultConnectInfo {
            connectHost = host
          , connectPort = port
        }
        return $ redisProvider connInfo
      destinations = fileDest <|> redisDest
      readInt :: String -> Integer
      readInt = read
    in case P.parse destinations "Destination" s of
      Left err -> fail $ "Invalid destination: " ++ show err
      Right x -> return x
