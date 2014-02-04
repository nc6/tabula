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
{-# LANGUAGE TemplateHaskell #-}
{-
Record data type. Designed to be imported qualified.
-}
module Tabula.Record where
  import Data.Aeson
  import Data.Aeson.TH
  import Data.Time.Clock

  data Record = Record {
      namespace :: String
    , version :: Int
    , timestamp :: UTCTime
    , entry :: Value
  }

  $(deriveJSON defaultOptions ''Record)

  class (ToJSON a, FromJSON a) => Recordable a where
    getNamespace :: a -> String
    getVersion :: a -> Int
    getTimestamp :: a -> UTCTime

  record :: (Recordable a) => a -> Record
  record a = Record 
                (getNamespace a)
                (getVersion a)
                (getTimestamp a)
                (toJSON a)