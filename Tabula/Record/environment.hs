{-# LANGUAGE DeriveGeneric #-}
-- | Types and functions dealing with environment variables.
module Tabula.Record.Environment (Env, EnvChange, diff) where

  import Control.Arrow ((***))
  import Control.Monad (join)

  import Data.Aeson (FromJSON, ToJSON)
  import qualified Data.Map.Strict as Map

  import GHC.Generics (Generic)

  type Key = String
  type Value = String

  type Env = [(Key, Value)]

  data Delta = Insert Key Value
             | Delete Key
             | Modify Key Value Value
              deriving (Eq, Show, Generic)

  instance FromJSON Delta
  instance ToJSON Delta

  type EnvChange = [Delta]

  diff :: Env -> Env -> EnvChange
  diff from to = let
      map2 = join (***)
      (f,t) = map2 Map.fromList (from, to)
      insertions = map (uncurry Insert) . Map.assocs $ Map.difference t f
      deletions = map Delete . Map.keys $ Map.difference f t
      modifications = Map.elems $ Map.intersectionWithKey Modify f t
    in insertions ++ deletions ++ modifications

