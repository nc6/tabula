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
-- Redis destination for tabula
{-# LANGUAGE LambdaCase,OverloadedStrings #-}
module Tabula.Destination.Redis (
      redisProvider
    , defaultConnectInfo
    , ConnectInfo(..)
  ) where
  import Control.Applicative
  import Control.Exception (evaluate, try)
  import Control.Monad (void)
  import Control.Monad.IO.Class

  import Data.Aeson
  import qualified Data.ByteString.Char8 as B
  import qualified Data.ByteString.Lazy as L
  import Data.Conduit
  import qualified Data.Conduit.List as DCL
  import Data.Maybe (catMaybes)
  import Database.Redis hiding (decode)

  import Tabula.Destination
  import Tabula.Record (Record)
  import qualified Text.Parsec as P

  showProject :: Project -> B.ByteString
  showProject (UserProject user key) = 
    B.intercalate ":" ["tabula", "project", B.pack user, B.pack key]
  showProject (GlobalProject key) = 
    B.intercalate ":" ["tabula", "project", "global", B.pack key]

  readProject :: (Monad m) => B.ByteString -> m Project
  readProject p = let
      (<+>) a b = (,) <$> a <*> b
      sep = P.char ':'
      ns = P.string "tabula" >> sep >> P.string "project" >> sep
      global = P.string "global" >> sep >> projectNameParser >>= return . GlobalProject
      username = P.many1 (P.alphaNum)
      local = (username <* sep) <+> projectNameParser >>= return . (uncurry UserProject)
      project = ns >> global <|> local
    in case P.parse project "Project" p of
      Left err -> fail $ "Invalid project form: " ++ show err
      Right x -> return x

  redisProvider :: ConnectInfo -> DestinationProvider
  redisProvider connInfo = DestinationProvider {
      listProjects = do
        conn <- connect connInfo
        projects <- runRedis conn $ keys "tabula:project*"
        case projects of
          Right bs -> mapM readProject $ bs
          Left _ -> fail "Problem fetching keys." -- TODO better errors!
    , projectDestination = redisDestination connInfo . showProject
    , removeProject = undefined
  }

  redisDestination :: ConnectInfo -> B.ByteString -> Destination
  redisDestination connInfo project = Destination {
      recordSink = redisSink connInfo project
    , getLastRecord = lastRecord connInfo project
    , recordSource = redisSource connInfo project
  }

  redisSink :: ConnectInfo -> B.ByteString -> Sink Record (ResourceT IO) ()
  redisSink connInfo project = bracketP
      (connect connInfo)
      (\conn -> void $ runRedis conn quit)
      (\conn -> loop conn)
    where 
      loop conn = awaitForever $ \rec -> let 
          recS = B.concat . L.toChunks $ encode rec
        in liftIO . void . tryR . runRedis conn $ do
            rpush project [recS]
      tryR = try . (>>= evaluate) :: IO a -> IO (Either ConnectionLostException a)

  -- Redis source. At the moment, just get all posts (should probably chunk nicely!)
  redisSource :: ConnectInfo -> B.ByteString -> Source (ResourceT IO) Record
  redisSource connInfo project = bracketP
      (connect connInfo)
      (\conn -> void $ runRedis conn quit)
      (\conn -> stream conn)
    where
      stream conn = do
        results <- liftIO . runRedis conn $ lrange project 0 (-1)
        case results of
          Right bs -> DCL.sourceList . catMaybes . map (decode . L.fromStrict) $ bs
          Left _ -> return ()

  lastRecord :: ConnectInfo -> B.ByteString -> IO (Maybe Record)
  lastRecord connInfo project = do
    conn <- connect connInfo
    thing <- runRedis conn $ lindex project (-1)
    case thing of
      Right (Just bs) -> return . decode . L.fromStrict $ bs
      _ -> return Nothing