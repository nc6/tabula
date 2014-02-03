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
    , Command(..)
    , Options
    , RecordOptions
    , readDestination
  ) where
  import Data.Char (toUpper)
  import Data.Vinyl
  import Database.Redis (PortID(PortNumber))

  import Options.Applicative hiding (command)
  import qualified Options.Applicative as Opt

  import System.Log (Priority(..))

  import Tabula.Destination
  import Tabula.Destination.File
  import Tabula.Destination.Redis
  import qualified Text.Parsec as P

  -------------- Options ------------------

  data Command = Record (PlainRec RecordOptions)
               | Cat (PlainRec CatOptions)

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
  -- | Project database. At the moment, this is just a directory.
  type T_db = "db" ::: Maybe (Project -> Destination)
  db = Field :: T_db

  -- Default options --
  type RecordOptions = [ "resume" ::: Bool
                          , T_db
                          , "bufferSize" ::: Int
                          , T_project
                        ]
  -- | Resume a session
  resume = Field :: "resume" ::: Bool
  -- | Set buffer size
  bufferSize = Field :: "bufferSize" ::: Int

  -- | Cat options
  type CatOptions = [ T_db, T_project ]

  --------------- Parsers ------------------

  version :: Parser (a -> a)
  version = infoOption "Tabula version 0.1.2.0"
    (  long "version"
    <> help "Print version information" )
  
  -- Shared options

  projectOption :: Parser String
  projectOption = argument str (metavar "PROJECT" <> value "default") 

  -- Option groups
  recordOptions :: Rec RecordOptions Parser
  recordOptions = resume <-: (switch (long "resume" <> help "Resume existing session."))
                <+> db <-: optional (nullOption (long "destination"
                            <> short 'd'
                            <> metavar "DESTINATION"
                            <> reader readDestination
                            <> help "Destination to write logs to."))
                <+> bufferSize <-: option (long "bufferSize"
                                             <> metavar "SIZE"
                                             <> value 64
                                             <> help "Set buffer size (in multiples of 4096B)")
                <+> project <-: projectOption

  catOptions :: Rec CatOptions Parser
  catOptions = db <-: optional (nullOption (long "destination"
                            <> short 'd'
                            <> metavar "DESTINATION"
                            <> reader readDestination
                            <> help "Destination to read logs from."))
              <+> project <-: projectOption

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
                (progDesc "Print a project session to stdout.")
            )
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

  readDestination :: Monad m => String -> m (Project -> Destination)
  readDestination s = let
      protoSep = P.string "://"
      path = P.many1 (P.noneOf ":")
      fileDest = P.string "file" >> protoSep >> do
        p <- path
        return $ fileDestination p
      redisDest =  P.string "redis" >> protoSep >> do
        host <- P.option (connectHost defaultConnectInfo) $ 
          P.many1 (P.alphaNum <|> P.char '.')
        port <- P.option (connectPort defaultConnectInfo) $ 
          liftA (PortNumber . fromIntegral . readInt) (P.char ':' >> P.many1 (P.digit))
        let connInfo = defaultConnectInfo {
            connectHost = host
          , connectPort = port
        }
        return $ redisDestination connInfo
      destinations = fileDest <|> redisDest
      readInt :: String -> Integer
      readInt = read
    in case P.parse destinations "Destination" s of
      Left err -> fail $ "Invalid destination: " ++ show err
      Right x -> return x
