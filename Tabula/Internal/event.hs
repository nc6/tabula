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

  $(deriveJSON defaultOptions ''Event)