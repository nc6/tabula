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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Tabula.Command.Cat (
    catSession
  , Format(..)
) where
  import Prelude hiding (init)
  import Control.Monad (unless)

  import Data.Aeson
  import Data.Aeson.Encode.Pretty (encodePretty)
  import qualified Data.ByteString as B
  import qualified Data.ByteString.Lazy.Char8 as LB
  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import qualified Data.Conduit.List as DCL

  import System.IO

  import Tabula.Destination
  import Tabula.Record (entry)
  import Tabula.Record.Console (command)

  data Format = Full | AsHistory

  -- | Print a project log to stdout. 
  catSession :: Destination -- ^ Session to fetch logs from
             -> Format -- ^ Show history commands only
             -> IO ()
  catSession dest fmt = runResourceT $ (recordSource dest) 
      =$= formatter
      =$= DCL.map (B.concat . LB.toChunks)
      $$  DCB.sinkHandle stdout
    where 
      formatter = case fmt of
        Full -> DCL.map encodePretty =$= mkString "[" "," "]\n"
        AsHistory -> DCL.map (fmap (LB.pack . command) . fromJSONMaybe . entry) 
          =$= DCL.catMaybes =$= mkString "" "\n" "\n"
      fromJSONMaybe a = case fromJSON a of
        Error _ -> Nothing
        Success b -> Just b

  mkString :: (Monad m) => 
              LB.ByteString -- ^ Initial marker
           -> LB.ByteString -- ^ Separation marker
           -> LB.ByteString -- ^ Terminal marker
           -> Conduit LB.ByteString m LB.ByteString
  mkString init sep term = yield init >> loop True >> yield term where
    loop first = await >>= \case
      Just a -> do
              unless first $ yield sep
              yield a
              loop False
      Nothing -> return ()