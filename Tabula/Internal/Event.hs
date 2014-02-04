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
Low-level events, which we will then sessionise to make records. These events come from
various handles or sent as messages through the network socket.
-}
module Tabula.Internal.Event where
  import Data.Aeson.TH
  import Data.ByteString (ByteString)
  import Data.Time.Clock

  type Env = [(String, String)]

  data Event = Stdout ByteString
    | Stdin ByteString
    | Stderr ByteString
    | Prompt UTCTime Env String String Int -- ^ timestamp env cwd command exitCode
    | Debug UTCTime String Int Int Env -- ^ timestamp command pid ppid environment
    | Stop
    deriving (Show, Eq)
    
  $(deriveJSON defaultOptions ''Event)