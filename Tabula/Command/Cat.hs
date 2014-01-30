{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Tabula.Command.Cat (
  catSession
) where
  import Prelude hiding (init)
  import Control.Monad (unless)

  import Data.Aeson.Encode.Pretty (encodePretty)
  import qualified Data.ByteString as B
  import qualified Data.ByteString.Lazy as LB
  import Data.Conduit
  import qualified Data.Conduit.Binary as DCB
  import qualified Data.Conduit.List as DCL

  import System.IO

  import Tabula.Destination

  -- | Print a project log to stdout. 
  catSession :: Destination -- ^ Session to fetch logs from
             -> IO ()
  catSession dest = runResourceT $ (recordSource dest) 
    =$= DCL.map encodePretty
    =$= mkString "[" "," "]"
    =$= DCL.map (B.concat . LB.toChunks)
    $$  DCB.sinkHandle stdout

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