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
{-# LANGUAGE DeriveGeneric #-}
-- | Types and functions dealing with environment variables.
module Tabula.Record.Environment (Env, EnvChange, diff, indifferentMerge) where

  import Control.Arrow ((***))
  import Control.Monad (join)

  import Data.Aeson (FromJSON, ToJSON)
  import Data.List (foldl')
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
      modifications = filter (\(Modify _ a b) -> a /= b) . Map.elems $ 
        Map.intersectionWithKey Modify f t
    in insertions ++ deletions ++ modifications

  {- | Merge in changes to an environment to produce a new environment.
      Note that this version of merge is indifferent to whether the changes
      are actually appropriate to the environment they are being applied to:

       - A modify will work even if the old value is different from the one
      present, or there is no value present.
       - An insert will act as a modify if a value is already there.
       - A delete will not care if the value it's deleting does not exist.

  -}
  indifferentMerge :: EnvChange -> Env -> Env
  indifferentMerge changes old = Map.toList $ 
      foldl' appDelta (Map.fromList old) changes 
    where
      appDelta env (Insert key value) = Map.insert key value env
      appDelta env (Modify key _ value) = Map.insert key value env
      appDelta env (Delete key) = Map.delete key env
